## TODO: mbind for matrix cound use sparse matrix code
## check for necessity of [,,drop=FALSE] in mbind for 1x cases
## check problem with merge for samp/taxa

## groupSums and groupMeans

setGeneric("groupSums", function(object, ...) standardGeneric("groupSums"))

## MARGIN indicates what to group (1: group rows, 2: group cols)
setMethod("groupSums", "matrix", function(object, MARGIN, by, na.rm = FALSE, ...) {
    if (any(is.na(by)))
        stop("'NA' not allowed in 'by'")
    if (any(is.na(object)) && !na.rm)
        stop("'NA' found in 'object'")
    if (!(MARGIN %in% 1:2))
        stop("'MARGIN' must be in 1:2")
    if (length(MARGIN) != 1)
        stop("MARGIN = 1:2 not yet implemented")
    if (length(by) != dim(object)[MARGIN])
        stop("Non conforming 'object', 'MARGIN' and 'by'")
    mm <- as(factor(by, levels=unique(by)), "sparseMatrix")
    rownames(mm) <- unique(by)
    object <- as(object, "sparseMatrix")
    if (na.rm)
        object[is.na(object)] <- 0
    if (MARGIN == 2) {
        out <- t(mm %*% t(object))
    } else {
        out <- mm %*% object
    }
    as.matrix(out)
})
setMethod("groupSums", "sparseMatrix", function(object, MARGIN, by, na.rm = FALSE, ...) {
    if (any(is.na(by)))
        stop("'NA' not allowed in 'by'")
    if (any(is.na(object)) && !na.rm)
        stop("'NA' found in 'object'")
    if (!(MARGIN %in% 1:2))
        stop("'MARGIN' must be in 1:2")
    if (length(MARGIN) != 1)
        stop("MARGIN = 1:2 not yet implemented")
    if (length(by) != dim(object)[MARGIN])
        stop("Non conforming 'object', 'MARGIN' and 'by'")
    mm <- as(factor(by, levels=unique(by)), "sparseMatrix")
    rownames(mm) <- unique(by)
    if (na.rm)
        object[is.na(object)] <- 0
    if (MARGIN == 2) {
        out <- t(mm %*% t(object))
    } else {
        out <- mm %*% object
    }
    out
})
## replace is a replacement object for the affected non xtab slot (samp, taxa)
setMethod("groupSums", "Mefa", function(object, MARGIN, by, replace, na.rm = FALSE, ...) {
    x <- groupSums(object@xtab, MARGIN, by, na.rm, ...)
    if (missing(replace))
        replace <- NULL
    if (MARGIN == 2) {
        new("Mefa", xtab = x, samp = object@samp,
            taxa = replace, join = object@join)
    } else {
        new("Mefa", xtab = x, samp = replace,
            taxa = object@taxa, join = object@join)
    }
})

setGeneric("groupMeans", function(object, ...) standardGeneric("groupMeans"))

setMethod("groupMeans", "sparseMatrix", function(object, MARGIN, by, na.rm = FALSE, ...) {
    x <- groupSums(object, MARGIN, by, na.rm, ...)
    out <- sweep(x, MARGIN, table(by), "/", check.margin = FALSE)
    as(out, "sparseMatrix")
})
setMethod("groupMeans", "matrix", function(object, MARGIN, by, na.rm = FALSE, ...) {
    as.matrix(groupMeans(as(object, "sparseMatrix"), MARGIN, by, na.rm, ...))
})
setMethod("groupMeans", "Mefa", function(object, MARGIN, by, replace, na.rm = FALSE, ...) {
    x <- groupMeans(as(object, "sparseMatrix"), MARGIN, by, na.rm, ...)
    if (missing(replace))
        replace <- NULL
    if (MARGIN == 2) {
        new("Mefa", xtab = x, samp = object@samp,
            taxa = replace, join = object@join)
    } else {
        new("Mefa", xtab = x, samp = replace,
            taxa = object@taxa, join = object@join)
    }
})

## mbind: joining 2 Mefa objects

setGeneric("mbind", function(x, y, fill, ...) standardGeneric("mbind"))

setMethod("mbind", signature(x="matrix", y="matrix", fill="ANY"), 
    function(x, y, fill, ...) {
        if (missing(fill))
            fill <- NA
        if (length(x) == 0)
            stop("length of 'x' must not be 0")
        if (length(y) == 0)
            stop("length of 'y' must not be 0")
        if (is.null(dimnames(x)))
            stop("dimnames of 'x' must not be NULL")
        if (is.null(dimnames(y)))
            stop("dimnames of 'y' must not be NULL")
        r1 <- rownames(x)
        c1 <- colnames(x)
        r2 <- rownames(y)
        c2 <- colnames(y)
        if (setequal(r1, r2) && setequal(c1, c2))
            return(x)
        rx <- setdiff(r1, r2)
        rxy <- intersect(r1, r2)
        ry <- setdiff(r2, r1)
        cx <- setdiff(c1, c2)
        cxy <- intersect(c1, c2)
        cy <- setdiff(c2, c1)
        xx <- x[c(rx, rxy), c(cx, cxy)]
        z1 <- matrix(fill, length(ry), length(cx))
        z2 <- matrix(fill, length(rx), length(cy))
        yx1 <- y[ry, cxy]
        yx2 <- y[rxy, cy]
        yy <- y[ry, cy]
        if (length(ry) > 0) {
            part1 <- cbind(z1, yx1)
            part2 <- rbind(xx, part1)
        } else part2 <- xx
        if (length(cy) > 0) {
            part3 <- rbind(z2, yx2)
            part4 <- rbind(part3, yy)
            part5 <- cbind(part2, part4)
        } else part5 <- part2
        rownames(part5) <- c(rx, rxy, ry)
        colnames(part5) <- c(cx, cxy, cy)
        part5
})
setMethod("mbind", signature(x="sparseMatrix", y="sparseMatrix", fill="ANY"), 
    function(x, y, fill, ...) {
        if (missing(fill))
            fill <- NA
        if (length(x) == 0)
            stop("length of 'x' must not be 0")
        if (length(y) == 0)
            stop("length of 'y' must not be 0")
        if (is.null(dimnames(x)))
            stop("dimnames of 'x' must not be NULL")
        if (is.null(dimnames(y)))
            stop("dimnames of 'y' must not be NULL")
        r1 <- rownames(x)
        c1 <- colnames(x)
        r2 <- rownames(y)
        c2 <- colnames(y)
        if (setequal(r1, r2) && setequal(c1, c2))
            return(x)
        rx <- setdiff(r1, r2)
        rxy <- intersect(r1, r2)
        ry <- setdiff(r2, r1)
        cx <- setdiff(c1, c2)
        cxy <- intersect(c1, c2)
        cy <- setdiff(c2, c1)
        xx <- x[c(rx, rxy), c(cx, cxy)]
        z1 <- as(matrix(fill, length(ry), length(cx)), "sparseMatrix")
        z2 <- as(matrix(fill, length(rx), length(cy)), "sparseMatrix")
        yx1 <- y[ry, cxy]
        yx2 <- y[rxy, cy]
        yy <- y[ry, cy]
        if (length(ry) > 0) {
            part1 <- cBind(z1, yx1)
            part2 <- rBind(xx, part1)
        } else part2 <- xx
        if (length(cy) > 0) {
            part3 <- rBind(z2, yx2)
            part4 <- rBind(part3, yy)
            part5 <- cBind(part2, part4)
        } else part5 <- part2
        rownames(part5) <- c(rx, rxy, ry)
        colnames(part5) <- c(cx, cxy, cy)
        part5
})
setMethod("mbind", signature(x="Mefa", y="Mefa", fill="ANY"), 
    function(x, y, fill, drop, ...) {
        if (missing(drop))
            drop <- FALSE
        if (missing(fill))
            fill <- NA
        ## xtab
        xtabx <- x@xtab
        xtaby <- y@xtab
        if (length(xtabx) == 0)
            stop("length of 'x' must not be 0")
        if (length(xtaby) == 0)
            stop("length of 'y' must not be 0")
        r1 <- rownames(xtabx)
        c1 <- colnames(xtabx)
        r2 <- rownames(xtaby)
        c2 <- colnames(xtaby)
        if (setequal(r1, r2) && setequal(c1, c2))
            return(x)
        rx <- setdiff(r1, r2)
        rxy <- intersect(r1, r2)
        ry <- setdiff(r2, r1)
        cx <- setdiff(c1, c2)
        cxy <- intersect(c1, c2)
        cy <- setdiff(c2, c1)
        xx <- xtabx[c(rx, rxy), c(cx, cxy)]
        z1 <- as(matrix(fill, length(ry), length(cx)), "sparseMatrix")
        z2 <- as(matrix(fill, length(rx), length(cy)), "sparseMatrix")
        yx1 <- xtaby[ry, cxy]
        yx2 <- xtaby[rxy, cy]
        yy <- xtaby[ry, cy]
        if (length(ry) > 0) {
            part1 <- cBind(z1, yx1)
            part2 <- rBind(xx, part1)
        } else part2 <- xx
        if (length(cy) > 0) {
            part3 <- rBind(z2, yx2)
            part4 <- rBind(part3, yy)
            part5 <- cBind(part2, part4)
        } else part5 <- part2
        rownames(part5) <- c(rx, rxy, ry)
        colnames(part5) <- c(cx, cxy, cy)
        ## samp
        sampx <- x@samp
        sampy <- y@samp
        if (is.null(sampx) && is.null(sampy))
            sm2 <- NULL
        if (!is.null(sampx) && !is.null(sampy)) {
            sampx2 <- data.frame(ROWNAMES=rownames(sampx), SAMPPART=1, sampx[,,drop=FALSE])
            sampy2 <- data.frame(ROWNAMES=rownames(sampy), SAMPPART=2, sampy[,,drop=FALSE])
            sm <- merge(sampx2, sampy2, all=TRUE)
            sid1 <- which(sm$SAMPPART==1 & sm$ROWNAMES %in% r1)
            sid2 <- which(sm$SAMPPART==2 & sm$ROWNAMES %in% ry)
            sm2 <- sm[c(sid1, sid2),]
            rownames(sm2) <- sm2$ROWNAMES
            sm2$ROWNAMES <- sm2$SAMPPART <- NULL
        }
        if (!is.null(sampx) && is.null(sampy))
            sm2 <- sampx
        if (is.null(sampx) && !is.null(sampy))
            sm2 <- sampy[ry,]
        ## taxa
        taxax <- x@taxa
        taxay <- y@taxa
        if (is.null(taxax) && is.null(taxay))
            tm2 <- NULL
        if (!is.null(taxax) && !is.null(taxay)) {
            taxax2 <- data.frame(ROWNAMES=rownames(taxax), TAXAPART=1, taxax[,,drop=FALSE])
            taxay2 <- data.frame(ROWNAMES=rownames(taxay), TAXAPART=2, taxay[,,drop=FALSE])
            ## 1: merge common cols only from y to x
            ## 2: add unique cols from y to tm
            tm <- merge(taxax2, taxay2, all=TRUE)
            tid1 <- which(tm$TAXAPART==1 & tm$ROWNAMES %in% c1)
            tid2 <- which(tm$TAXAPART==2 & tm$ROWNAMES %in% cy)
            tm2 <- tm[c(tid1, tid2),]
            rownames(tm2) <- tm2$ROWNAMES
            tm2$ROWNAMES <- tm2$TAXAPART <- NULL
        }
        if (!is.null(taxax) && is.null(taxay))
            tm2 <- taxax
        if (is.null(taxax) && !is.null(taxay))
            tm2 <- taxay[cy,]
        ## assembling
        Mefa(part5, sm2, tm2, join="left", drop)
})
