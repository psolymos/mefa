`[.mefa` <-
function (x, i=1:dim(x)[1], j=1:dim(x)[2], k=1:dim(x)[3], drop=FALSE)
{
    if (length(i) == 0 || length(j) == 0 || length(k) == 0)
        return(NULL)
    if (length(i) == 1 || length(j) == 1) {
        warning("dimensions are too few to be extracted")
        return(x)}
    subsegm <- if (!is.null(x$segm))
        length(k) != length(x$segm) else FALSE
    x$call <- match.call()

    if (is.null(x$segm)) {
        x$segm <- NULL} else {
        segm.out <- list()
        for (nn in 1:length(k))
            segm.out[[nn]] <- x$segm[[k[nn]]][i, j]
        names(segm.out) <- names(x$segm)[k]
        }

    if (!subsegm) {
        xtab.out <- x$xtab[i, j]
        segm.out <- x$segm
        } else {
        if (!is.null(x$segm) && length(k) == 1) {
            xtab.out <- segm.out[[1]]
            } else {
                if (attr(x, "nested")) {
                    xtab.out <- x$segm[[max(k)]]
                    } else {
                    xtab.out <- segm.out[[1]]
                    for (nn in 2:length(k))
                        xtab.out <- xtab.out + segm.out[[nn]] }
            }
        }
    x$segm <- segm.out
    x$xtab <- xtab.out
    if (is.null(x$samp))
        x$samp <- NULL else {
        x$samp <- x$samp[i, ]
        if (drop) x$samp[] <- lapply(x$samp, function(x) x[drop = TRUE])}
    if (is.null(x$taxa))
        x$taxa <- NULL else {
        x$taxa <- x$taxa[j, ]
        if (drop) x$taxa[] <- lapply(x$taxa, function(x) x[drop = TRUE])}
    return(x)
}

