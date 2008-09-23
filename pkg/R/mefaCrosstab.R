`mefaCrosstab` <-
function(x, segment=FALSE, nested=FALSE, drop.zero=FALSE)
{
    if (!is.stcs(x))
        stop("x must be of class stcs")
    if (!attr(x, "expand")) {
        ss <- stcs(x, expand = TRUE, drop.zero=drop.zero)
        } else ss <- x
    if (!segment) {
        out <- as.matrix(table(ss$samp, ss$taxa))
        if (attr(ss, "zero.count") && !drop.zero)
            out <- out[, -which(colnames(out) %in% attr(ss, "zero.pseudo"))]}
    if (segment) {
        out <- list()
        nsegm <- if (attr(ss, "zero.count"))
            nlevels(ss$segm) - 1 else nlevels(ss$segm)
        if (nsegm == 1) {
            out <- NULL
            } else {
            for (i in 1:nsegm) {
                if (drop.zero || (levels(ss$segm)[i] != attr(ss, "zero.pseudo"))) {
                    out[[i]] <- as.matrix(table(ss$samp, ss$tax, ss$segm)[,,i])
                if (attr(ss, "zero.count") && !drop.zero)
                    out[[i]] <- out[[i]][, -which(colnames(out[[i]]) %in% attr(ss, "zero.pseudo"))]
                names(out)[i] <- levels(ss$segm)[i]}}
            if (nested) {
                lnam <- names(out)
                for (i in 2:length(out)) {
                    out[[i]] <- out[[(i-1)]] + out[[i]]
                    names(out)[[i]] <- paste(lnam[1], "-", lnam[i], sep="")}}
            }
        }
return(out)
}

