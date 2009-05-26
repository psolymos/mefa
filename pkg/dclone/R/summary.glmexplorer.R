summary.glmexplorer <-
function(object, crit=0.05, ...)
{
    if (attr(object, "select")) {
        est <- lapply(object, function(z) z[,1])
        nam <- unique(unlist(lapply(object, rownames)))
        rval <- matrix(NA, length(nam), length(est),
            dimnames=list(nam, names(est)))
        for (i in 1:length(est)) {
            rval[rownames(object[[i]]),i] <- est[[i]]
        }
    } else {
        rval <- as.data.frame(lapply(object, function(z) z[,1]))
        rownames(rval) <- rownames(object[[1]])
        pval <- as.data.frame(lapply(object, function(z) z[,4]))
        rval <- data.matrix(rval)
        pval <- data.matrix(pval)
        if (rownames(object[[1]])[1] == "(Intercept)")
            pval[1,] <- 0
        rval[pval >= crit] <- NA
    }
    rval
}
