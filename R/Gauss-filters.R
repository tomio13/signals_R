# suggested to be added

convolve.fft <- function(x,y, padding='', is.filter= TRUE) {
    #' convolution using FFT with nonzero padding
    #' @details
    #' When convolution filter is used for smoothing data, data and the
    #' kernel are zero padded traditionally. However, this causes a cut-off
    #' at the edges, which is often an undesired effect.
    #'
    #' This version pads the first function with its first and last value
    #' elminating cut off to 0 if these are non zero.
    #' This way a smoothing will work better, but such a treatment should not
    #' be done for other convolution operations, like autocorrelation.
    #' Thus, the switch is.filter activates this feature.
    #'
    #' One more word on padding: the signal is padded in a symmetric manner,
    #' the kernel only to its end. Then the resulted range is cut out of the
    #' returned convolved data.
    #' Only the real part of the complex result is returned
    #' (we assume x and y are real)
    #'
    #' @param x,y float arrays to be convolved
    #' @param padding int|text the padding would be len(x)+len(y) by default
    #'                  if padding is 'full', then go up to the next
    #'                  power of 2 in length
    #' @param is.filter bool if TRUE, we do non-zero padding for filter application
    #'                          y is then the filter kernel
    #'
    #' @return      the convolved array with length of x
    #
    #'
    #' @export

    # Simplified the code just to focus on padding. Reversing the kernel
    # is now in for any case.

    N.x <- length(x)
    N.y <- length(y)
    N.y.2 <- floor(N.y/2)

    if (padding == 'full') {
        # we pad up to next power of 2
        n <- ceiling(log(N.x+2*N.y)/log(2))
        N.tab <- 2**n
        # cat('Padding up to', N.tab,'\n')
        # and fill the first curve symmetric
        # leave the other asymmetric
        N.diff <- floor((N.tab-N.x)/2)
    } else {
        #cat('Using padding only at kernel length', N.y, 'on both sides\n')
        N.tab <- N.x + 2*N.y
        N.diff <- N.y # (N.tab - N.x)/2
    }

    if (is.filter == TRUE) {
        # nonzero padding on both sides
        const.0 <- x[1]
        const.N <- tail(x, 1)
    } else {
        # the classical zero padding
        const.0 <- 0
        const.N <- 0
    }
    # signal is padded symmetrically:
    x.c <- c(rep(const.0, N.diff), x, rep(const.N, N.tab - N.x - N.diff))
    # kernel is padded only on one side
    # so mirroring kernel comes here...
    y.c <- c(rev(y), rep(0, N.tab - N.y))

    # we cut out the relevant part from the result:
    # from: half padding + half kernel + 1
    # to one signal length further
    #
    # and normalize with the array length (needed for FFT)
    x.res <- fft(fft(x.c)*fft(y.c), inverse=TRUE)[(N.diff+N.y.2+1):(N.diff+N.y.2+N.x)]/N.tab
    return (Re(x.res))
}


gauss.kernel <- function(sigma, R= NULL, deriv=0) {
    #' generate a 1D Gaussian kernel
    #' @details
    #' Calculate the kernel from the Gaussian
    #' using the equation 1/sqrt(2*pi)/sigma*exp(-(x)**2/(2*sigma**2))
    #' Alternatively, use the derivative function that is -x/sigma**2 times
    #' the Gaussian above.
    #'
    #' @param sigma float, standard deviation
    #' @param R integer, number of points; if NULL, then use 3*sigma
    #' @param deriv integer 0 or 1, the order of derivative to be used
    #'
    #' @return array containing the Gaussian
    #'
    #' @export

    if (is.null(R)) {
        R <- 3*sigma
    }
    x <- (-R):R
    y <- 1/sqrt(2*pi)/sigma*exp(-(x)**2/(2*sigma**2))
    if( deriv == 1){
        y <- -y * (x)/sigma**2
    }
    return(list(x=x, y=y))
}


smooth.gauss <- function(y, sigma= 10, R= NULL, deriv=0) {
    #' A function to smooth the data array y using a Gaussian kernel
    #' @details
    #' This function uses an non-zero padding and the convolve.fft function
    #' to calculate the result.
    #' The mean is subtracted from y for removing an offset.
    #' If y is only smoothed, this mean is readded at the end. For the
    #' derivative it is left out.
    #'
    #' If R (window radius) is not more than 3*sigma, the kernel is renormalized to avoid
    #' offsetting the data.
    #' The full window is 2R+1 wide.
    #' The derivative (deriv= 1) is calculated with the negative of the
    #' derivative of the Gaussian kernel, because we need the kernel be positive first
    #' then negative. This is equivalent to a smoothing then using the diff(), but returns
    #' an array with the same length as y.
    #'
    #' @param y     the data
    #' @param sigma integer, width of the Gaussian, standard deviation
    #' @param R     integer, the half window size for the kernel; if NULL,
    #'              use automatic, 3*sigma see gauss.kernel
    #' @param deriv integer, derivative, possible values 0 or 1
    #'
    #' @return array of smoothed data
    #'
    #' @export

    if(is.null(R)) {
        R = 3*sigma
    }
    gk <- gauss.kernel(sigma, R, deriv)
    # the real derivative is negative at the start,
    # not what we need in the detection.
    # leaving out the rev() in the fft convolution would
    # eliminate the - gk$y here, but then it would behave
    # strange for other kernels...
    if (deriv == 1) {
        gk$y <- - gk$y
    }

    y.mean <- mean(y)
    # renormalize the kernel:
    # this is not necessary if R is c.a. > 3*w
    if (deriv == 0 && R < 3*sigma){
        gky <- gk$y - min(gk$y)
        # we can do real integral instead of sum only:
        gk$y <- gky/sum(diff(gk$x)*(gky[-1]+gky[-length(gky)])/2)
        # or simply the sum:
        # gk$y <- gky/sum(gky)
    }
    # shift y, which is fine for both normal and deriv values
    # is.filter will ensure non-zero padding of the signal
    y.res <- convolve.fft(y-y.mean, gk$y, is.filter= TRUE)

    # if we smoothen, push y back to the original
    if (deriv == 0){
        y.res <- y.res + y.mean
    }
    return (y.res)
}


diff.gauss <- function(y, sigma= 10, R= NULL) {
    #' differentiate data using a Gaussian kernel
    #' @details
    #' It is an envelope function calling the smooth.Gauss with deriv=1
    #'
    #' @param y     the data vector
    #' @param sigma width of the Gaussian (equivalent to
    #'              the standard deviation of a normal distribution)
    #' @param R     half width of the window used in the convolution
    #'              Window size is: 2R+1
    #'
    #' @return  the differentiated vector with the same length as y
    #'
    #' @export

    smooth.gauss(y, sigma, R, deriv=1)
}


find.peaks <- function(a,
                       sigma= 3,
                       R= NULL,
                       peak.threshold= 0.05,
                       index.only= FALSE,
                       peak.window= 2,
                       both = FALSE,
                       verbose= FALSE) {
    #' Find peaks in a dataset
    #' @details
    #' find fast and dirty peak candidates, and their positions.
    #' The idea is to take the first derivative using a Gaussian differential
    #' kernel, and then find the zero transitions from positive to negative
    #' slopes.
    #' We check if peaks are closer than peak.window, and take the maximum
    #' within this range (cutting secondary peaks off).
    #' The function can return the index of a peak, its weighted position and
    #' the hight of the local maximum.
    #'
    #' @param a     two column array, [x,y]
    #' @param sigma width for the differential kernel
    #' @param R     window radius for the differential kernel (optional)
    #' @param peak.threshold float a relative minimum height of the peaks
    #'             (compared to the maximum of y)
    #' @param index.only    boolean, if TRUE return the index of the peaks
    #' @param peak.window int, how many points are taken before and after
    #'          the peak to refine its position as a sum(x*y)/sum(y)
    #'          and find the maximum of y within this range.
    #' @param both  boolean, if set return a list with pos and index
    #' @param verbose, boolean, if set plot the derivative and selections
    #'
    #' @return  an array of peak posiions in X or a list with index, position
    #'          and peak height arrays (y.max). If indx.only is set, return
    #'          an array of index positions
    #'
    #' @export

    x <- a[,1]
    y <- a[,2]
    dy <- diff.gauss(y, sigma, R)
    # this is where the derivative crosses from + to -:
    indx.1 <- y[-1] > peak.threshold*max(y)
    indx.2 <- diff(sign(dy)) < -1
    indx <- which(indx.2 & indx.1)
    if (verbose) {
        cat('max:', max(y), 'thresh:', peak.threshold*max(y),'\n')
        cat(which(indx.1),'\n')
        cat('diff < 2:', which(indx.2), '\n')
        cat('remained:', indx,'\n')

        plot(x, dy, type='o')
        # blue points are above peak.threshold
        points(x[indx.1], dy[indx.1], col='blue', pch=15)
        points(x[indx], dy[indx], col='orange', cex=1.5, pch=16)
    }

    if (index.only) {
        return(indx)
    }

    pos <- as.array(0, dim=c(length(indx),1))
    y.max <- as.array(0, dim=c(length(indx),1))

    # refine a bit:
    # eliminate indices within peak.window distance
    # diff() is 1 to the left, which will result in the lower peak index
    ell.indx <- which(diff(indx) <= peak.window) + 1
    if (length(ell.indx) > 0) {
        # ell.indx shows always the second hit
        if (verbose){
            points(x[indx[ell.indx]], dy[indx[ell.indx]], col='red', pch= 16)
            cat('diff indx:', diff(indx),'\n')
            cat('elliminated:', ell.indx, '\n')
        }
        indx <- indx[-ell.indx]
    }

    j <- 1
    for (i in indx) {
        i0 <- max(0, i - peak.window)
        i1 <- min(length(y), i + peak.window)
        yy <- y[i0:i1] - min(y[i0:i1])
        pos[j] <- sum(yy*x[i0:i1])/sum(yy)
        y.max[j] <- max(y[i0:i1])
        j <- j+1
    }
    if (verbose){
        abline(h= 0, col='green')
        abline(v= pos, col='red')
    }
    if (both) {
        return(list(index= indx, pos= pos, y.max= y.max))
    }

    return(pos)
}


background.gauss <- function(y, sigma= 10, N= 10) {
    #' Calculate a background based on smoothing the data
    #" using a Gaussian filter.
    #'
    #' @details
    #' Run the filter with sigma standard deviation N times,
    #' and in every keep the lower value between the filtered
    #' curve and the one before this filter step.
    #' The resuls is the smoothed background curve.
    #'
    #' @param y   the original curve
    #' @param sigma the standard deviation / width of the Gaussian
    #'              in points / index values, not X
    #' @param N     integer the number of runs
    #'
    #' @return an array of the background data
    #'
    #' @examples
    #' # Create an array with some peaks and noise
    #' x <- seq(0, 4*pi, len=200)
    #' set.seed(1234)
    #' y <- 2*sin(x) + 2*cos(3*x) - 0.5*(x-2*pi)**2
    #' plot(x,y)
    #' lines(x, background.gauss(y, 3,10), col='blue')
    #' # you may want to remove the jumps to lower noisy points
    #' lines(x, smooth.gauss(background.gauss(y, 3,10), 2), col='red')
    #' @export

    y.bg <- y
    N.y <- length(y)
    # keep.indx <- rep(TRUE, length(x))
    for (i in 1:N) {
        bg <- smooth.gauss(y.bg, sigma)
        # indx <- (y.bg > bg)
        #y.bg[indx] <- bg[indx]
        y.bg <- apply(array(c(y.bg, bg), dim=c(N.y,2)),
                      1,
                      min)
        # keep.indx[indx] <- FALSE
    }
    # return(list(x= x, y=y.bg, indx= keep.indx))
    return(y.bg)
}


integ.trap <- function(x,y) {
    #' calculate a simple trapezoid formula numerical integral
    #'
    #' @param x,y arrays of the same length
    #'
    #' @return the integral
    #'
    #' @export

    N <- length(x)
    return(
           sum(0.5*diff(x)*(y[-1]+y[-N]))
           )
}
