`aggregate.mefa` <-
function(x, by.samp=NULL, by.taxa=NULL)
{
    if (is.null(by.samp) && is.null(by.taxa))
        return(x)
    xtab <- as.data.frame(x$xtab)
    if (!is.null(by.samp)) {
        if (length(unique(by.samp)) == 1)
            stop("by.samp should contain at least 2 levels")
        x$samp <- NULL
        xtab <- aggregate(xtab, list(by.samp), sum)
        rownames(xtab) <- xtab[,1]
        xtab[,1] <- NULL
        if (!is.null(x$segm)){
            for (i in 1:length(x$segm)) {
                x$segm[[i]] <- aggregate(x$segm[[i]], list(by.samp), sum)
                rownames(x$segm[[i]]) <- x$segm[[i]][,1]
                x$segm[[i]][,1] <- NULL}}
        }
    if (!is.null(by.taxa)) {
        if (length(unique(by.taxa)) == 1)
            stop("by.taxa should contain at least 2 levels")
        x$taxa <- NULL
        xtab <- aggregate(t(xtab), list(by.taxa), sum)
        rownames(xtab) <- xtab[,1]
        xtab[,1] <- NULL
        xtab <- t(xtab)
        if (!is.null(x$segm)){
            for (i in 1:length(x$segm)) {
                x$segm[[i]] <- aggregate(t(x$segm[[i]]), list(by.taxa), sum)
                rownames(x$segm[[i]]) <- x$segm[[i]][,1]
                x$segm[[i]][,1] <- NULL
                x$segm[[i]] <- t(x$segm[[i]])}}
        }
    x$xtab <- as.matrix(xtab)
    x$call <- match.call()
    return(x)
}

