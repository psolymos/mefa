`plot.mefa` <-
function(x, which=1:4, type=c("hist", "rank"), trafo=c("none", "log",
"ratio"), ylab=NULL, xlab=NULL, show=TRUE, ...)
{
   if (!all(which %in% 1:4))
       stop("which must be in 1:4")
   if (!length(which) == 1) which <- 1
   if (!length(type) == 1) type <- type[1]
   if (!length(trafo) == 1) trafo <- trafo[1]
   type <- match.arg(type, c("hist", "rank"))
   trafo <- match.arg(trafo, c("none", "log", "ratio"))

   if (!is.null(ylab))
       ylab2 <- ylab
   if (!is.null(xlab))
       xlab2 <- xlab
   if (is.null(ylab) && type=="hist")
       ylab2 <- "Frequency"
   if (is.null(ylab) && type=="rank")
       ylab2 <- "Rank"

   if (which == 1) {
       if (is.null(ylab)) ylab2 <- paste(ylab2, "(samples)")
       if (is.null(xlab)) xlab2 <- "Number of taxa"
       yvar <- summary(x)$srich}
   if (which == 2) {
       if (is.null(ylab)) ylab2 <- paste(ylab2, "(samples)")
       if (is.null(xlab)) xlab2 <- "Number of individuals"
       yvar <- summary(x)$ninds}
   if (which == 3) {
       if (is.null(ylab)) ylab2 <- paste(ylab2, "(taxa)")
       if (is.null(xlab)) xlab2 <- "Occupancy"
       yvar <- summary(x)$spocc}
   if (which == 4) {
       if (is.null(ylab)) ylab2 <- paste(ylab2, "(taxa)")
       if (is.null(xlab)) xlab2 <- "Abundance"
       yvar <- summary(x)$spabu}

   if (trafo=="log") {
       yvar <- log10(yvar)
       if (is.null(ylab) && type=="hist")
           ylab2 <- paste("log10", ylab2)
       if (is.null(ylab) && type=="rank")
           xlab2 <- paste("log10", xlab2)}
   if (trafo=="ratio") {
       yvar <- yvar / max(yvar)
       if (is.null(ylab) && type=="hist")
           ylab2 <- paste("Relative", tolower(ylab2))
       if (is.null(ylab) && type=="rank")
           xlab2 <- paste("Relative", tolower(xlab2))}


       if (type=="hist") {
           yvar <- table(yvar)
           if (show)
                plot(yvar, xlab=xlab2, ylab=ylab2, ...)}
       if (type=="rank") {
           yvar <- yvar[order(yvar, decreasing=TRUE)]
           if (show)
                plot(yvar, type="l", xlab=ylab2, ylab=xlab2, ...)}

   invisible(yvar)
}
