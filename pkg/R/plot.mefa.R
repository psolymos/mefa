`plot.mefa` <-
function(x, which=1:4, ylab=NULL, xlab=NULL, ...)
{
    if (!all(which %in% 1:4))
        stop("which must be in 1:4")
    if (!length(which) == 1) which <- 1
    if (which == 1) {
        if (is.null(ylab)) ylab <- "Frequency (samples)"
        if (is.null(xlab)) xlab <- "Number of taxa"
        plot(table(summary(x)$srich), xlab=xlab, ylab=ylab, ...)}
    if (which == 2) {
        if (is.null(ylab)) ylab <- "Frequency (samples)"
        if (is.null(xlab)) xlab <- "Number of individuals"
        plot(table(summary(x)$srich), xlab=xlab, ylab=ylab, ...)}
    if (which == 3) {
        if (is.null(ylab)) ylab <- "Frequency (taxa)"
        if (is.null(xlab)) xlab <- "Occupancy"
        plot(table(summary(x)$spocc), xlab=xlab, ylab=ylab, ...)}
    if (which == 4) {
        if (is.null(ylab)) ylab <- "Frequency (taxa)"
        if (is.null(xlab)) xlab <- "Abundance"
        plot(table(summary(x)$spabu), xlab=xlab, ylab=ylab, ...)}
    invisible()
}

