samp.mefa <- function(x, summaries=FALSE, ...)
    if (is.null(x$samp))
        return(NULL) else if (summaries)
            as.data.frame(x, fun=mss, ...) else x$samp
