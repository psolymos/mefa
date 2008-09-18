`print.summary.mefa` <-
function(x, nlist=10, ...)
{
    mstat <- cbind(summary(x[[1]]), summary(x[[2]]),
        summary(x[[3]]), summary(x[[4]]))
    colnames(mstat) <- c("S richnness", "N indsividuals", "Occupancy", "Abundance")
    Summary <- c(x$ntot, round(x$mfill*100), x$nsamp, x$ntaxa, x$nsegm)
    names(Summary) <- c("Total sum", "Matrix fill (%)", "Number of samples", "Number of taxa", "Number of segments")

    cat("\nCall:\n", sep = "")
    print(x$call, ...)
    cat("\n")
    print(data.frame(Summary), ...)
    if(x$segm.orig) cat("\n") else {
        seglist <- x$segment
        if (length(seglist) > nlist)
            seglist <- c(seglist[1:nlist], "[...]")
        cat("\nSegments:\n", sep="")
        cat(seglist, sep=", ")
        cat("\n\n")}
    print(mstat, ...)
    cat("\n")

    invisible(x)
}

