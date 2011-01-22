## class definitions

#setClass("MefaCall", representation(call = "language"))
#setClass("Xtab", contains = c("MefaCall","dgCMatrix"))
setClass("Xtab", contains = "dgCMatrix")
setClassUnion("XtabMatrix", c("matrix","Xtab","dgCMatrix"))
setClass("Mefa", 
    representation(
#        call = "language",
        xtab = "XtabMatrix",
        samp = "data.frame",
        taxa = "data.frame",
        join = "character"),
    prototype = list(
        xtab = as(matrix(0,0,0), "dgCMatrix"),
        samp = data.frame(),
        taxa = data.frame(),
        join = "left"),
    validity = function(object) {
        if (is.null(dim(object@xtab)))
            return("'xtab' must have a 'dim' attribute")
        if (length(dim(object@xtab)) != 2)
            return("'xtab' dimension must be 2")
        if (identical(object@samp, data.frame()) &&
            identical(rownames(object@xtab), rownames(object@samp)))
            return("Inconsistent 'xtab' and 'samp' dimnames")
        if (identical(object@taxa, data.frame()) &&
            identical(colnames(object@xtab), rownames(object@taxa)))
            return("Inconsistent 'xtab' and 'xtab' dimnames")
        if (!(object@join %in% c("left", "inner")))
            return("'join' must be in c(\"left\", \"inner\")")
        if (identical(object@samp, data.frame()) &&
            identical(object@taxa, data.frame()) &&
            object@join != "inner")
            return("'join' must be \"inner\" if both 'samp' and 'taxa' slots are empty")
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
        samp <- data.frame()
        sid <- rownames(xtab)
    } else sid <- rownames(samp)
    if (missing(taxa)) {
        taxa <- data.frame()
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
        rkeep <- intersect(xrid, sid)
        ckeep <- intersect(xcid, tid)
    }
    xtab <- xtab[rkeep, ckeep]
    samp <- samp[rkeep,]
    taxa <- taxa[ckeep,]
    rownames(samp) <- rkeep
    rownames(taxa) <- ckeep
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
setMethod("xtab", "Mefa", function(x) x@xtab)
setMethod("samp", "Mefa", function(x) x@samp)
setMethod("taxa", "Mefa", function(x) x@taxa)
setMethod("dim", "Mefa", function(x) dim(x@xtab))
setMethod("dimnames", "Mefa", function(x) dimnames(x@xtab))

## setters

setGeneric("xtab<-", function(x, value) standardGeneric("xtab<-"))
setGeneric("samp<-", function(x, value) standardGeneric("samp<-"))
setGeneric("taxa<-", function(x, value) standardGeneric("taxa<-"))
setReplaceMethod("xtab", "Mefa", function(x, value) {
    value <- as(value, "dgCMatrix")
    if (x@join == "left") {
        rkeep <- rownames(value)
        ckeep <- colnames(value)
        x@xtab <- value
        x@samp <- x@samp[rkeep,]
        x@taxa <- x@taxa[ckeep,]
    }
    if (x@join == "inner") {
        rkeep <- intersect(rownames(value), rownames(x@samp))
        ckeep <- intersect(colnames(value), rownames(x@taxa))
        x@xtab <- value[rkeep, ckeep]
        x@samp <- x@samp[rkeep,]
        x@taxa <- x@taxa[ckeep,]
    }
    rownames(x@samp) <- rkeep
    rownames(x@taxa) <- ckeep
    x
})
setReplaceMethod("samp", "Mefa", function(x, value) {
    if (x@join == "left") {
        rkeep <- rownames(x@xtab)
        x@samp <- x@samp[rkeep,]
    }
    if (x@join == "inner") {
        rkeep <- intersect(rownames(x@xtab), rownames(value))
        x@xtab <- value[rkeep,]
        x@samp <- x@samp[rkeep,]
    }
    rownames(x@samp) <- rkeep
    x
})
setReplaceMethod("taxa", "Mefa", function(x, value) {
    if (x@join == "left") {
        ckeep <- colnames(x@xtab)
        x@taxa <- x@taxa[ckeep,]
    }
    if (x@join == "inner") {
        ckeep <- intersect(colnames(x@xtab), rownames(value))
        x@xtab <- value[,ckeep]
        x@taxa <- x@taxa[ckeep,]
    }
    rownames(x@taxa) <- ckeep
    x
})
setMethod("[", "Mefa", 
    function(x, i, j, ..., drop){
        d <- dim(x)
        if (missing(i))
            i <- 1:d[1]
        if (missing(j))
            j <- 1:d[2]
        if (missing(drop))
            drop <- FALSE
        x@xtab <- x@xtab[i,j]
        x@samp <- x@samp[i,,drop=drop]
        x@taxa <- x@taxa[j,,drop=drop]
        x
})

## coercion

setMethod("as.matrix", "Mefa", function(x) as.matrix(x@xtab))
#setMethod("as.array", "Mefa", function(x) as.array(x@xtab))
setAs(from = "Xtab", to = "Mefa", def = function(from) Mefa(from))
setAs(from = "Mefa", to = "Xtab", def = function(from) from@xtab)
