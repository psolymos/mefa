segm.mefa <- function(x, segments=1:dim(x)[3], ...) {
    if (!all(segments %in% 1:dim(x)[3]))
        stop("'segments' out of range")
    if (is.null(x$xtab))
        x$xtab else x$segm[segments]
}
