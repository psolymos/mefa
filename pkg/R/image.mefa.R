`image.mefa` <-
function(x, segm=NULL, trafo=c("none", "log", "bins", "prab"), probs = seq(0, 1, 0.05), ordering=TRUE, reverse=TRUE, ylab=NULL, xlab=NULL, show=TRUE, ...)
{
if (!is.mefa(x))
    stop("object is not of class 'mefa'")
if (length(trafo) > 1) trafo <- trafo[1]
trafo <- match.arg(trafo, c("none", "log", "bins", "prab"))
m <- if (!is.null(segm))
    x$segm[[segm]] else x$xtab
if (ordering)
    m <- m[order(rowSums(x$xtab)), order(colSums(x$xtab),decreasing=TRUE)]

mm <- t(m)

if (trafo == "log")
    mm <- log10(mm)
if (trafo == "bins")
    mm <- matrix(qvector(array(mm), probs), nrow(mm), ncol(mm))
if (trafo == "prab")
    mm <- matrix(as.numeric(mm > 0), nrow(mm), ncol(mm))
if (reverse)
    mm <- max(mm) - mm

if (is.null(ylab)) ylab <- "Samples"
if (is.null(xlab)) xlab <- "Taxa"

if (show)
    image(1:ncol(m), 1:nrow(m), mm, ylab=ylab, xlab=xlab, ...)
if (show)
    invisible() else return(mm)
}
