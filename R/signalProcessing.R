# basic function to load files, do calculations on the data

applyto.all <- function(pattern, f.call, prefix='res', ...) {
    #' take all variables based on pattern,
    #' and apply the function on them, then
    #' save the results back prepending prefix and '.' to their names
    #'
    #' @param pattern   text, the variables to work on
    #' @param f.call    a function to apply, first parameter is the
    #'                  variable to be applied on. All extra parameters
    #'                  are passed to this function
    #' @param prefix    text, prepend this to the result names
    #' @param ...       all further parameters are passed to the specified function
    #'
    #' @return nothing
    #'
    #' @export

    lst <- ls(pattern= pattern, envir= .GlobalEnv)
    if (length(lst) == 0) {
        cat('variables with pattern:', pattern, 'not found\n')
        return()
    }
    for (i in seq_along(lst)){
        cat('running on', lst[i], '\n')
        a <- get(lst[i], envir= .GlobalEnv)

        if (!is.null(a)){
            b <- f.call(a, ...)
            if (prefix != '' && ! is.null(prefix)){
                assign(paste(prefix, lst[i], sep='.'),
                   b,
                   envir= .GlobalEnv
                )
            } else {
                cat('result was not saved without prefix\n')
            }
        } else {
            cat('variable', lst[i], 'is empty\n');
        }
    }
    return()
}


read.all.files <- function(folder='./', pattern='.*\\.csv$',
                           reader= read.table,
                           prefix= '',
                           remove= '',
                           remove.extension= TRUE,
                           remove.dir= TRUE,
                           recursive= FALSE,
                           ...
                           ) {
    #' find a list of files based on folder and pattern via dir(),
    #' apply reader(path, ...) to read them
    #' convert their names to variable names using prefix and remove patterns
    #' add them to the global environment
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
        return
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
            assign(namelst[i], a, envir= .GlobalEnv)
        }
    }

    print(namelst)
}
