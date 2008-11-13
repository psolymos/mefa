`image.stcs` <-
function(x, segm=NULL, trafo=c("none", "log", "bins", "prab"), 
probs = seq(0, 1, 0.05), ordering=TRUE, reverse=TRUE, 
ylab=NULL, xlab=NULL, show=TRUE, ...)
{
image.mefa(mefa(x), segm, trafo, probs, ordering, reverse, ylab, xlab, show, ...)
}
