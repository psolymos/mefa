`mefa` <-
function(xtab, samp=NULL, taxa=NULL, id.samp=NULL, id.taxa=NULL, segment=TRUE, nested=FALSE,
drop.zero=FALSE, drop.index=FALSE, xtab.fixed=TRUE)
{
    x <- xtab
    if (min(dim(as.matrix(x))) == 1)
        stop("xtab should have dimensions at leat 2*2")
    if (any(is.na(x)))
        stop("xtab contains NA")
    if (nested && !segment)
        warning("nested = TRUE has no effect if segment = FALSE")
    if (!is.stcs(x)) {
        if (is.null(rownames(x)) && !is.null(samp))
            stop("rownames(x) should not be NULL")
        if (is.null(colnames(x)) && !is.null(taxa))
            stop("colnames(x) should not be NULL")
        xtab <- as.matrix(x)
        segm <- NULL}

    if (is.stcs(x)) {
        xtab <- mefaCrosstab(x, segment=FALSE, drop.zero=drop.zero)
        segm <- if (segment) {
            mefaCrosstab(x, segment=TRUE, nested=nested, drop.zero=drop.zero)
            } else {NULL}}

    xtab2 <- xtab
    if (!is.null(samp)) {
        if (dim(as.matrix(samp))[2] == 1)
            stop("at least 2 columns needed for samp")
        samp.list <- mefaTables(xtab2, samp, 1, id.samp,
            drop.index=drop.index, xtab.fixed=xtab.fixed)
        xtab2 <- samp.list$xtab
        samp2 <- samp.list$dtab
        } else {
        samp2 <- NULL}

    if (!is.null(taxa)) {
        if (dim(as.matrix(taxa))[2] == 1)
            stop("at least 2 columns needed for taxa")
        taxa.list <- mefaTables(xtab2, taxa, 2, id.taxa,
            drop.index=drop.index, xtab.fixed=xtab.fixed)
        xtab2 <- taxa.list$xtab
        taxa2 <- taxa.list$dtab
        } else {
        taxa2 <- NULL}

    if (segment && !is.null(segm) && !xtab.fixed) {
        row.sub <- which(rownames(segm[[1]]) %in% rownames(xtab2))
        col.sub <- which(colnames(segm[[1]]) %in% colnames(xtab2))
        for (i in 1:length(segm)) {
            segm[[i]] <- segm[[i]][row.sub, col.sub]}}

    out <- list(call = match.call(), xtab = xtab2, segm = segm, samp = samp2, taxa = taxa2)
    class(out) <- c("mefa")
    attr(out, "nested") <- nested
    attr(out, "drop.zero") <- drop.zero
    attr(out, "xtab.fixed") <- xtab.fixed
    return(out)
}

