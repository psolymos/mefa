`melt.mefa` <-
function (x, segm.var, by.samp=TRUE, raw.out=FALSE, drop.zero=FALSE, ...)
{
    if (by.samp) {
        if (is.null(x$samp))
            stop("$samp is NULL")
        count <- array(t(x$xtab))
        samp <- rep(dimnames(x)[[1]], each=dim(x)[2])
        taxa <- rep(dimnames(x)[[2]], dim(x)[1])
        if (length(segm.var) > 1)
            segm.var2 <- interaction(x$samp[, segm.var])
            else segm.var2 <- x$samp[, segm.var]
        segm <- rep(segm.var2, each=dim(x)[2])
        } else {
        if (is.null(x$taxa))
            stop("$taxa is NULL")
        count <- array(x$xtab)
        samp <- rep(dimnames(x)[[1]], dim(x)[2])
        taxa <- rep(dimnames(x)[[2]], each=dim(x)[1])
        if (length(segm.var) > 1)
            segm.var2 <- interaction(x$taxa[, segm.var])
            else segm.var2 <- x$taxa[, segm.var]
        segm <- rep(segm.var2, dim(x)[1])
        }
    out <- data.frame(samp=samp, taxa=taxa, count=count, segm=segm)
    if (raw.out) {
        if (drop.zero){
            out <- out[out[ ,3] != 0, ]
            out[] <- lapply(out, function(x) x[drop = TRUE])}
        return(out)
    } else {
        cpart <- out[out[ ,3] != 0, ]
        csamp <- unique(as.character(out[out[ ,3] != 0, 1]))
        zsamp <- as.character(unique(out[out[ ,3] == 0, 1]))
        zsamp <- zsamp[!(zsamp %in% csamp)]
        n <- length(zsamp)
        zpart <- data.frame(samp=zsamp, taxa=rep("zero.pseudo", n),
            count=rep(0, n), segm=rep("zero.pseudo", n))
        out <- merge(cpart, zpart, all = TRUE)
        return(stsc(out, drop.zero=drop.zero, ...))}
}

