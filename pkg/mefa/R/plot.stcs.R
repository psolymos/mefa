`plot.stcs` <-
function(x, stat=1:4, type=c("bar", "rank"), trafo=c("none", "log",
"ratio"), ylab=NULL, xlab=NULL, show=TRUE, ...)
{
plot.mefa(mefa(x), stat, type, trafo, ylab, xlab, show, ...)
}
