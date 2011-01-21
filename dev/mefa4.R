## a proposed set of S4 classes for the 'mefa4' package
## this is only 2D meta objects (3D sparse representation 
## is not readily available)
## focus is on efficiency and structural checks to ensure
## data integrity and efficient memory usage.

## class definitions
library(Matrix)

setClass("MefaCall", representation(call = "language"))
setClass("Xtab", contains = c("MefaCall","dgCMatrix"))
setClassUnion("XtabMatrix", c("matrix","Xtab","dgCMatrix"))
setClass("Mefa", 
    representation(
        call = "language",
        xtab = "XtabMatrix",
        samp = "data.frame",
        taxa = "data.frame"),
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
    Call <- new("MefaCall", call = match.call())
    new("Xtab", Call, out)
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
    if (join == "inner") {
        rkeep <- intersect(xrid, sid)
        ckeep <- intersect(xcid, tid)
    }
    if (join == "left") {
        rkeep <- xrid
        ckeep <- xcid
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
    new("Mefa", call = match.call(),
        xtab = as(xtab, "dgCMatrix"), 
        samp = samp, taxa = taxa)
}

x <- data.frame(
    sample = paste("Sample", c(1,1,2,2,3,4), sep="."),
    species = c(paste("Species", c(1,1,1,2,3), sep="."),  "zero.pseudo"),
    count = c(1,2,10,3,4,0),
    segment = letters[c(6,13,6,13,6,6)])
samp <- data.frame(samples=levels(x$sample), val=1)
taxa <- data.frame(taxa=levels(x$species), val=2)
rownames(samp) <- samp$samples
rownames(taxa) <- taxa$taxa
x0 <- xtabs(count ~ sample + species, x)
x1 <- Xtab(count ~ sample + species, x)
x2 <- Xtab(count ~ sample + species, x, cdrop=F,rdrop=T)
Mefa(as.matrix(x1), samp, taxa)
Mefa(as(as.matrix(x1), "dgCMatrix"), samp, taxa)
Mefa(x1, samp, taxa)
Mefa(x2, samp, taxa)
Mefa(x1, samp[1:2,], taxa)
Mefa(x1, samp[1:2,],join="inner")
Mefa(x1)
x3 <- Mefa(as.matrix(x1), samp[1:2,], taxa)
x3@xtab <- 1:5
x3@samp <- samp[2:1,]
## still not OK, representation only checks for class, but not for proper structure
## to overcome: use proper extractor/giver function: 'xtab', 'xtab<-'

## TODO
## methods to add to
update
call
xtab, xtab<-
samp, samp<-
taxa, taxa<-
show, print, summary
plot, image (see image plot for Matrix classes)
[<-, [[<- ([[<- should set the slots via proper extractors)
dim
dimnames
?? set a class union for XtabMatrix and Mefa
?? which operatons should affect @call? -- update all the time
