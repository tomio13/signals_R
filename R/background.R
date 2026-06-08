lin.background <- function(x, y= NULL, q= 0.5, verbose= FALSE){
    #' @details
    #' background correction to remove linear backgrounds
    #' Data sets often have a linear drift. To estimate this drift, the
    #' lower part of the y data are taken into account and fitted using lm()
    #' This background can be then subtracted.
    #'
    #' @param x, y array of data
    #' @param y array or NULL the second column if x is single column
    #' @param q float, a quantile unter which the values are used for fitting
    #'  by default it is 0.5 -> median
    #' @param verbose Boolean, produce a plot showing the background and the fit
    #'
    #' @return returns a list with the new values, x,y and the fit object ft
    #'
    #' @export

    if(is.null(y)){
        y <- x
        x <- 1:length(y)

    } else if( length(x) != length(y)){
        cat('Array lengths mismatch\n')
        return(list())
    }

    if( q <= 0 || q >= 1){
        cat('Quantile is between (0,1). q=',q,'is invalid\n');
        return(list())
    }

    indx <- y < quantile(y, q)
    if(sum(indx) < 2){
        cat("Background not found!\n")
        return(list(x=x, y=y))
    }
    b.x <- x[indx]
    b.y <- y[indx]
    b.ft <- lm( b.y ~ 1+ b.x)

    y.new <- y - x*coef(b.ft)[2] - coef(b.ft)[1]
    if(verbose){
        plot(x,y, main='background correction')
        points(b.x, b.y, col='blue')
        lines(b.x, predict(b.ft), col='red')
    }
    return(list(x= x, y= y.new, ft= b.ft))
}


background <- function(y, width= 20) {
    #' @details
    #' Calculate a background of a data set assuming this background
    #' is a lower envelop of the curve represented in the 'y' array.
    #' The process works by taking the minimum between the actual data
    #' point and the average of two points at the distance of +/- j,
    #' thus employing a window of 2j +1.
    #' This procedure is repeated for j = 1 ... width.
    #'
    #' @references
    #' Based on the work of:
    #' Miroslav Morhác and Vladislav Matousek,
    #' Peak clipping algorithms for background estimation in
    #' spectroscopic data
    #' Applied Spectroscopy 62(1): 91 - 106 (2008)
    #'
    #' @param y  array, the data to be approximated
    #' @param width float the half width of the window
    #'
    #' @return array of new points
    #'
    #' @export

    N <- length(y)
    s <- y
    for (j in 1:width) {
        # current valid data length is N - j -j (front and tail)
        N.j <- N - 2*j
        filt <- array(c(
                        s[j:(N-j-1)],
                        0.5*(head(s, N.j) + tail(s, N.j))
                        ),
                      dim = c(N.j, 2)
                      )
        # for every position we take the minimum of y[j] or
        # the mean of its neighbors
        s[j:(N-j-1)] <- apply(filt, 1, min)
    }
    return(s)
}
