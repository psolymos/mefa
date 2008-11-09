`boxplot.stcs` <-
function(x, stat=1:4, ylab=NULL, xlab=NULL, show=TRUE, ...)
{
boxplot.mefa(mefa(x), stat, ylab, xlab, show, ...)
}

