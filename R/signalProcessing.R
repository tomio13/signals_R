# basic function to load files, do calculations on the data

process.ls <- function(pattern,
                       f.call,
                       prefix='res',
                       return.list= TRUE,
                       envir= .GlobalEnv,
                       ...) {
    #' process data based on a name pattern and export the results
    #' into the global environment
    #' @details
    #' take all variables based on pattern,
    #' and apply the function on them, then
    #' save the results back prepending prefix and '.' to their names
    #' and/or create a list of the results and return it.
    #'
    #' While it is a bad practice to emit results into the global environment,
    #' this is a shorthand for looping for the foul.
    #' Practically one can just do the looping as:
    #'  sapply(ls(pattern= pattern), function(n)\{ a <- get(n); ....\})
    #'
    #' to get simple results, but if your called function creates something
    #' more complicated, e.g. a list, you may want to put it back to the memory
    #' and not spend manual work on it. This is where this function comes handy.
    #'
    #' @param pattern   text, the variables to work on
    #' @param f.call    a function to apply, first parameter is the
    #'                  variable to be applied on. All extra parameters
    #'                  are passed to this function
    #' @param prefix    text, prepend this to the result names
    #' @param return.list Boolean, if TRUE return an invisible list of results indexed
    #'                    using the name of found variables.
    #' @param envir     environment, the environment to work with. Both the input and
    #'                  output are in this environment.
    #' @param ...       all further parameters are passed to the specified function
    #'
    #' @return nothing
    #'
    #' @export

    lst <- ls(pattern= pattern, envir= envir)
    if (length(lst) == 0) {
        cat('variables with pattern:', pattern, 'not found\n')
        return()
    }
    res <- list()

    for (i in seq_along(lst)){
        cat('running on', lst[i], '\n')
        a <- get(lst[i], envir= envir)

        if (!is.null(a)){
            b <- f.call(a, ...)
            if (prefix != '' && ! is.null(prefix)){
                assign(paste(prefix, lst[i], sep='.'),
                   b,
                   envir= envir
                )
            } else {
                cat('result was not saved without prefix\n')
            }

            if (return.list) {
                res[[i]] <- b
            }
        } else {
            cat('variable', lst[i], 'is empty\n');
        }
    }
    if (return.list) {
        invisible(res)
    }
    return(res)
}


read.all.files <- function(folder='./', pattern='.*\\.csv$',
                           reader= read.table,
                           prefix= '',
                           remove= '',
                           remove.extension= TRUE,
                           remove.dir= TRUE,
                           recursive= FALSE,
                           envir= .GlobalEnv,
                           ...
                           ) {
    #' apply a reader function to a list of filenames found by dir()
    #' @details
    #' find a list of files based on folder and pattern via dir(),
    #' apply reader(path, ...) to read them
    #' convert their names to variable names using prefix and remove patterns
    #' add them to the named environment.
    #' in all file names convert - or _ characters to .-s, and turn multiple '.'
    #' to single ones.
    #'
    #' @param folder    text, the folder to search in
    #' @param pattern   text, the file name pattern to load
    #' @param reader    function, returns the object read
    #' @param prefix    text, prepend this to the file names
    #' @param remove    text, pattern to be removed from the names
    #' @param remove.extension  Boolean, remove the last dot and its tail
    #' @param remove.dir Boolean strip path from filenames
    #' @param recursive Boolean, run into subfolders or not
    #' @param envir     environment, where to put the results
    #' @param ...       all further parameters are passed to the reader function
    #'
    #' @return nothing
    #'
    #' @export

    lst <- dir(folder,
               pattern= pattern,
               recursive= recursive,
               full.names = TRUE)

    if (length(lst) < 1) {
        cat('Nothing was found!\n')
        return()
    }

    if (remove.extension) {
        namelst <- gsub('\\.[a-z A-Z]*$', '', lst)
    } else {
        namelst <- lst
    }

    if (remove.dir) {
        namelst <- basename(namelst)
    }

    if (remove != '') {
        namelst <- gsub(remove, '', namelst)
    }

    # now, handle the remaining -_ characters
    namelst <- gsub('[ _-]+', '.', namelst)
    namelst <- gsub('\\.+', '.', namelst)
    if (prefix != '') {
        namelst <- paste(prefix, namelst, sep='.')
    }

    for (i in seq_along(lst)){
        a <- reader(lst[i], ...)
        if (!is.null(a)) {
            assign(namelst[i], a, envir= envir)
            # show what was created:
            cat(namelst[i],'\n')
        }
    }

    return()
}
