`boxplot.mefa` <-
function(x, which=1:4, ylab=NULL, xlab=NULL, ...)
{
    if (is.null(x$segm) || dim(x)[3] == 1)
        stop("at least 2 segments needed")
    if (!all(which %in% 1:4))
        stop("which must be in 1:4")
    if (!length(which) == 1) which <- 1
    yval <- list()
    for (i in 1:dim(x)[3])
        yval[[i]] <- summary(mefa(x$segm[[i]]))[[which]]
    yval <- unlist(yval)
    xval <- as.factor(rep(dimnames(x)$segm, each=length(summary(x)[[which]])))
    if (is.null(ylab))
        ylab <- c("Frequency (samples)", "Frequency (samples)",
        "Frequency (taxa)", "Frequency (taxa)")[which]
    if (is.null(xlab))
        xlab <- c("Number of taxa", "Number of individuals",
        "Occupancy", "Abundance")[which]
    plot(yval ~ xval, xlab=xlab, ylab=ylab, ...)
    invisible()
}

