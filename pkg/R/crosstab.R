`crosstab` <-
function(x, segment=FALSE, nested=FALSE, drop.zero=FALSE)
{
    if (!is.stsc(x))
        stop("x must be of class stsc")
    if (!attr(x, "expand"))
        ss <- stsc(x, expand = TRUE, drop.zero=drop.zero)
    if (!segment) {
        out <- as.matrix(table(ss$samp, ss$taxa))
        if (!is.null(attr(ss, "zero.pseudo")) && !drop.zero)
            out <- out[, -which(colnames(out) %in% attr(ss, "zero.pseudo"))]}
    if (segment) {
        out <- list()
        nsegm <- if (!is.null(attr(ss, "zero.pseudo")))
            nlevels(ss$segm) - 1 else nlevels(ss$segm)
        if (nsegm == 1) out <- NULL
            else {
            for (i in 1:nsegm) {
                if (drop.zero || levels(ss$segm)[i] != attr(ss, "zero.pseudo")) {
                    out[[i]] <- as.matrix(table(ss$samp, ss$tax, ss$segm)[,,i])
                if (!is.null(attr(ss, "zero.pseudo")) && !drop.zero)
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

