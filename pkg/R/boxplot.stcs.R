`boxplot.stcs` <-
function(x, stat=1:4, all = TRUE, ylab=NULL, xlab=NULL, show=TRUE, ...)
{
boxplot.mefa(mefa(x), stat, all, ylab, xlab, show, ...)
}

