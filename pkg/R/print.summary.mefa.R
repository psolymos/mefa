`print.summary.mefa` <-
function(x, nlist, ...)
{
    if (missing(nlist))
        nlist <- attr(x, "nlist")
    mstat <- cbind(summary(x[[1]]), summary(x[[2]]),
        summary(x[[3]]), summary(x[[4]]))
    colnames(mstat) <- c("s.rich", "s.abu", "t.occ", "t.abu")
    Summary <- c(x$ntot, round(x$mfill*100), x$nsamp, x$ntaxa, x$nsegm)
    names(Summary) <- c("Total sum", "Matrix fill (%)", "Number of samples", "Number of taxa", "Number of segments")

    cat("\nCall:\n", sep = "")
    print(x$call, ...)
    cat("\n")
    print(data.frame(Summary), ...)
    seglist <- x$segment
    if (length(seglist) > nlist)
        seglist <- c(seglist[1:nlist], "[...]")
    if (length(seglist) == 1)
        cat("\nSegment:\n", sep="") else cat("\nSegments:\n", sep="")
    cat(seglist, sep=", ")
    cat("\n\n")
    print(mstat, ...)
    cat("\n")

    invisible(x)
}

