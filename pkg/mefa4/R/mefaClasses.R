## class definitions

#setClass("MefaCall", representation(call = "language"))
#setClass("Xtab", contains = c("MefaCall","dgCMatrix"))
setClass("Xtab", contains = "dgCMatrix")
## class unions
setClassUnion("MefaMatrix", c("matrix","Xtab","dgCMatrix"))
setClassUnion("MefaDataFrame", c("data.frame","NULL"))
## virtual classes to old mefa classes
setClass("stcs", representation("VIRTUAL"))
setClass("mefa", representation("VIRTUAL"))
## main Mefa class
setClass("Mefa", 
    representation(
#        call = "language",
        xtab = "MefaMatrix",
        samp = "MefaDataFrame",
        taxa = "MefaDataFrame",
        join = "character"),
    prototype = list(
        xtab = as(matrix(0,0,0), "dgCMatrix"),
        samp = NULL,
        taxa = NULL,
        join = "left"),
    validity = function(object) {
        if (is.null(dim(object@xtab)))
            return("'xtab' must have a 'dim' attribute")
        if (length(dim(object@xtab)) != 2)
            return("'xtab' dimension must be 2")
        if (!is.null(object@samp) &&
            !identical(rownames(object@xtab), rownames(object@samp)))
            return("Inconsistent 'xtab' and 'samp' dimnames")
        if (!is.null(object@taxa) &&
            !identical(colnames(object@xtab), rownames(object@taxa)))
            return("Inconsistent 'xtab' and 'xtab' dimnames")
        if (!(object@join %in% c("left", "inner")))
            return("'join' must be in c(\"left\", \"inner\")")
        if (is.null(object@samp) &&
            is.null(object@taxa) &&
            object@join != "left")
            return("'join' must be \"left\" if both 'samp' and 'taxa' slots are 'NULL'")
        TRUE
    })

## creator functions

Xtab <-
function(formula = ~., data = parent.frame(), 
rdrop, cdrop,
subset, na.action, exclude = c(NA, NaN), drop.unused.levels = FALSE)
{
    ## this code is taken from Matrix::xtabs
    ## and modified by P Solymos

    ## cannott use only data, formula is needed
    if (missing(formula)) 
        stop("must supply 'formula'")
    if (!missing(formula)) {
        formula <- as.formula(formula)
        if (!inherits(formula, "formula")) 
            stop("'formula' missing or incorrect")
    }
    if (any(attr(terms(formula, data = data), "order") > 1)) 
        stop("interactions are not allowed")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- m$exclude <- m$drop.unused.levels <- m$sparse <- NULL
    m$rdrop <- m$cdrop <- NULL
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    if (length(formula) == 2) {
        by <- mf
        y <- NULL
    }
    else {
        i <- attr(attr(mf, "terms"), "response")
        by <- mf[-i]
        y <- mf[[i]]
    }
    by <- lapply(by, function(u) {
        if (!is.factor(u)) 
            u <- factor(u, exclude = exclude)
        u[, drop = drop.unused.levels]
    })
    if (length(by) != 2)
        stop("'sparse = TRUE' applies only to two-way tables")
    rows <- by[[1]]
    cols <- by[[2]]
    rl <- levels(rows)
    cl <- levels(cols)
    if (is.null(y)) 
        y <- rep.int(1, length(rows))
    ## this is how it is constructed, then converted into dgCMatrix
    out <- as(new("dgTMatrix", i = as.integer(rows) - 1L, j = as.integer(cols) - 
        1L, x = as.double(y), Dim = c(length(rl), length(cl)), 
        Dimnames = list(rl, cl)), "CsparseMatrix")
    rkeep <- 1:nrow(out)
    ckeep <- 1:ncol(out)
    if (!missing(rdrop)) {
        if (is.logical(rdrop)) {
            if (rdrop)
                rkeep <- which(rowSums(out) > 0)
        } else if (is.numeric(rdrop)) {
            rkeep <- setdiff(rkeep, rdrop)
        } else if (is.character(rdrop)) {
            rkeep <- setdiff(rownames(out), rdrop)
        } else stop("inappropriate 'rdrop' value")
    }
    if (!missing(cdrop)) {
        if (is.logical(cdrop)) {
            if (cdrop)
                ckeep <- which(colSums(out) > 0)
        } else if (is.numeric(cdrop)) {
            ckeep <- setdiff(ckeep, cdrop)
        } else if (is.character(cdrop)) {
            ckeep <- setdiff(colnames(out), cdrop)
        } else stop("inappropriate 'cdrop' value")
    }
    out <- out[rkeep, ckeep]
    out <- drop0(out)
    new("Xtab", out)
#    Call <- new("MefaCall", call = match.call())
#    new("Xtab", Call, out)
#    new("Xtab", match.call(), out)
}

Mefa <-
function(xtab, samp, taxa,
join = c("left", "inner"), drop = FALSE) {
    if (missing(xtab))
        stop("'xtab' must be supplied")
    if (missing(samp)) {
#        samp <- data.frame()
        samp <- NULL
        sid <- rownames(xtab)
    } else sid <- rownames(samp)
    if (missing(taxa)) {
#        taxa <- data.frame()
        taxa <- NULL
        tid <- colnames(xtab)
    } else tid <- rownames(taxa)
    xrid <- rownames(xtab)
    xcid <- colnames(xtab)
    join <- match.arg(join)
    if (join == "left") {
        rkeep <- xrid
        ckeep <- xcid
    }
    if (join == "inner") {
        rkeep <- if (!is.null(samp))
            intersect(xrid, sid) else xrid
        ckeep <- if (!is.null(taxa))
            intersect(xcid, tid) else xcid
    }
    xtab <- xtab[rkeep, ckeep]
    if (!is.null(samp)) {
        samp <- samp[rkeep,]
        rownames(samp) <- rkeep
    }
    if (!is.null(taxa)) {
        taxa <- taxa[ckeep,]
        rownames(taxa) <- ckeep
    }
    if (drop) {
        samp[] <- lapply(samp, function(z) z[drop = TRUE])
        taxa[] <- lapply(taxa, function(z) z[drop = TRUE])
    }
    new("Mefa", 
#        call = match.call(),
        xtab = as(xtab, "dgCMatrix"), 
        samp = samp, taxa = taxa,
        join = join)
}

## accessor methods 

setGeneric("xtab", function(x) standardGeneric("xtab"))
setGeneric("samp", function(x) standardGeneric("samp"))
setGeneric("taxa", function(x) standardGeneric("taxa"))
setMethod("xtab", signature(x = "Mefa"), function(x) x@xtab)
setMethod("samp", signature(x = "Mefa"), function(x) x@samp)
setMethod("taxa", signature(x = "Mefa"), function(x) x@taxa)
## this is required for subsetters
#setMethod("length", signature(x = "Mefa"), function(x) length(x@xtab))
## old mefa classes
setMethod("xtab", signature(x = "mefa"), function(x) x$xtab)
setMethod("samp", signature(x = "mefa"), function(x) x$samp)
setMethod("taxa", signature(x = "mefa"), function(x) x$taxa)


## setters

setGeneric("xtab<-", function(x, value) standardGeneric("xtab<-"))
setGeneric("samp<-", function(x, value) standardGeneric("samp<-"))
setGeneric("taxa<-", function(x, value) standardGeneric("taxa<-"))
setReplaceMethod("xtab", signature(x = "Mefa", value = "MefaMatrix"),
    function(x, value) {
        value <- as(value, "dgCMatrix")
        if (x@join == "left") {
            rkeep <- rownames(value)
            ckeep <- colnames(value)
            x@xtab <- value
            if (!is.null(x@samp))
                x@samp <- x@samp[rkeep,]
            if (!is.null(x@taxa))
                x@taxa <- x@taxa[ckeep,]
        }
        if (x@join == "inner") {
            rkeep <- if (!is.null(x@samp)) {
                intersect(rownames(value), rownames(x@samp))
            } else {
                rownames(value)
            }
            ckeep <- if (!is.null(x@taxa)) {
                intersect(colnames(value), rownames(x@taxa))
            } else {
                colnames(value)
            }
            x@xtab <- value[rkeep, ckeep]
            if (!is.null(x@samp))
                x@samp <- x@samp[rkeep,]
            if (!is.null(x@taxa))
                x@taxa <- x@taxa[ckeep,]
        }
        if (!is.null(x@samp))
            rownames(x@samp) <- rkeep
        if (!is.null(x@taxa))
            rownames(x@taxa) <- ckeep
        x
})
setReplaceMethod("samp", signature(x = "Mefa", value = "MefaDataFrame"), 
    function(x, value) {
        if (!is.null(value)) {
            if (x@join == "left") {
                rkeep <- rownames(x@xtab)
                x@samp <- value[rkeep,]
            }
            if (x@join == "inner") {
                rkeep <- intersect(rownames(x@xtab), rownames(value))
                x@xtab <- x@xtab[rkeep,]
                x@samp <- value[rkeep,]
            }
            rownames(x@samp) <- rkeep
        } else x@samp <- NULL
        x
})
setReplaceMethod("taxa", signature(x = "Mefa", value = "MefaDataFrame"), 
    function(x, value) {
        if (!is.null(value)) {
            if (x@join == "left") {
                ckeep <- colnames(x@xtab)
                x@taxa <- value[ckeep,]
            }
            if (x@join == "inner") {
                ckeep <- intersect(colnames(x@xtab), rownames(value))
                x@xtab <- x@xtab[,ckeep]
                x@taxa <- value[ckeep,]
            }
            rownames(x@taxa) <- ckeep
        } else x@taxa <- NULL
        x
})

setMethod("[", signature(x = "Mefa", i = "ANY", 
        j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {
        if (missing(i))
            i <- 1:dim(x)[1]
        if (missing(j))
            j <- 1:dim(x)[2]
        if (missing(drop))
            drop <- FALSE
        if (any(is.na(i)))
            stop("index contains 'NA'")
        if (any(is.na(j)))
            stop("index contains 'NA'")
        x@xtab <- x@xtab[i,j]
        if (!is.null(x@samp)) {
            x@samp <- x@samp[i,]
            if (drop)
                x@samp <- lapply(x@samp, function(z) z[drop=TRUE])
        }
        if (!is.null(x@taxa)) {
            x@taxa <- x@taxa[j,]
            if (drop)
                x@taxa <- lapply(x@taxa, function(z) z[drop=TRUE])
        }
        x
})

## coercion

setMethod("as.matrix", "Mefa", function(x) as.matrix(x@xtab))
#setMethod("as.array", "Mefa", function(x) as.array(x@xtab))
setAs(from = "Xtab", to = "Mefa", def = function(from) Mefa(from))
setAs(from = "Mefa", to = "Xtab", def = function(from) as(from@xtab,"Xtab"))
setAs(from = "matrix", to = "Xtab", def = function(from) as(as(from,"sparseMatrix"),"Xtab"))
setAs(from = "matrix", to = "Mefa", def = function(from) Mefa(from))

## general methods

setMethod("dim", "Mefa", function(x) dim(x@xtab))
setMethod("dimnames", "Mefa", function(x) dimnames(x@xtab))
setMethod("dimnames<-", signature(x = "Mefa", value = "list"), 
    function(x, value) {
        dimnames(x@xtab) <- value
#        x@xtab@Dimnames <- value
        if (!is.null(x@samp))
            rownames(x@samp) <- value[[1]]
        if (!is.null(x@taxa))
            rownames(x@taxa) <- value[[2]]
        x
})
## transpose, why not?
setMethod("t", "Mefa", function(x) {
    new("Mefa", xtab = t(x@xtab),
        samp = x@taxa, taxa = x@samp,
        join = x@join)
})

setMethod("show", "Mefa", function(object) {
    d <- dim(object)
    cat("Object of class \"Mefa\"\n")
    cat("  ..@ xtab:", d[1], "x", d[2], "sparse Matrix\n")
    if (!is.null(object@samp)) {
        cat("  ..@ samp: data frame with", ncol(object@samp), "variables\n")
    } else {
        cat("  ..@ samp: NULL\n")
    }
    if (!is.null(object@taxa)) {
        cat("  ..@ taxa: data frame with", ncol(object@taxa), "variables\n")
    } else {
        cat("  ..@ taxa: NULL\n")
    }
    cat("  ..@ join:", object@join, "\n")
    invisible(object)
})

## groupSums and groupMeans

setGeneric("groupSums", function(object, ...) standardGeneric("groupSums"))
## MARGIN ndicates what to group (1: group rows, 2: group cols)
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
setMethod("groupSums", "Xtab", function(object, MARGIN, by, na.rm = FALSE, ...) {
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
    new("Xtab", out)
})
## replace is a replacement object for the affected non xtab slot (samp, taxa)
setMethod("groupSums", "Mefa", function(object, MARGIN, by, replace, na.rm = FALSE, ...) {
    x <- new("Xtab", groupSums(new("Xtab", object@xtab), MARGIN, by, na.rm, ...))
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
setMethod("groupMeans", "Xtab", function(object, MARGIN, by, na.rm = FALSE, ...) {
    x <- groupSums(object, MARGIN, by, na.rm, ...)
    out <- sweep(x, MARGIN, table(by), "/", check.margin = FALSE)
    new("Xtab", as(out, "dgCMatrix"))
})
setMethod("groupMeans", "matrix", function(object, MARGIN, by, na.rm = FALSE, ...) {
    as.matrix(groupMeans(as(object, "Xtab"), MARGIN, by, na.rm, ...))
})
setMethod("groupMeans", "Mefa", function(object, MARGIN, by, replace, na.rm = FALSE, ...) {
    x <- groupMeans(as(object, "Xtab"), MARGIN, by, na.rm, ...)
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

