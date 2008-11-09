`boxplot.mefa` <-
function(x, stat=1:4, ylab=NULL, xlab=NULL, show=TRUE, ...)
{
    if (is.null(x$segm) || dim(x)[3] == 1)
        stop("at least 2 segments needed")
    if (!all(stat %in% 1:4))
        stop("'stat' must be in 1:4")
    if (!length(stat) == 1) stat <- 1

    yval <- list()
    for (i in 1:dim(x)[3])
        yval[[i]] <- summary(mefa(x$segm[[i]]))[[stat]]
    yval <- unlist(yval)
    xval <- as.factor(rep(dimnames(x)$segm, each=length(summary(x)[[stat]])))
    if (is.null(ylab))
        ylab <- c("Number of taxa", "Number of individuals",
        "Frequency of occurrence", "Abundance")[stat]
    if (is.null(xlab))
        xlab <- "Segments"
    if (show)
        boxplot(yval ~ xval, xlab=xlab, ylab=ylab, ...)
    if (show)
# invisibly returns plotted walues
        invisible() else return(cbind(x=xval, y=yval))
}

