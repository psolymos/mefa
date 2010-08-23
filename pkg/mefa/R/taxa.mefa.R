taxa.mefa <- function(x, summaries=FALSE, ...)
    if (is.null(x$taxa))
        return(NULL) else if (summaries)
            as.data.frame(x, fun=mts, ...) else x$taxa
