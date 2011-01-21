## class definitions
library(mefa)
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

## performance

b <- read.csv("c:/Dropbox/pkg/mefa4/RAW_T26BreedingBirds_10843.csv")
## NONE: no species was recorded (8)
## SNI: Species Not Identified (216)
## VNA: Variable Not Applicable (6)
## DNC: Did Not Collect (13)
## PNA: Protocol Not Available (0 in this case)

## S3 mefa processing
b3 <- b
b3 <- b3[!(b3$Scientific.Name %in% c("VNA", "DNC", "PNA")),]
levels(b3$Scientific.Name)[levels(b3$Scientific.Name) %in% c("NONE", "SNI")] <- "zero.pseudo"
b3$Counts <- ifelse(b3$Scientific.Name == "zero.pseudo", 0, 1)
b3$Label <- with(b3, paste(ABMI.Site, Year, Point.Count.Station, sep="_"))
x3 <- b3[!duplicated(b3$Label), c("Label", "ABMI.Site", "Year", "Field.Date", "Point.Count.Station",
    "Wind.Conditions", "Precipitation")]
rownames(x3) <- x3$Label
z3 <- b3[!duplicated(b3$Scientific.Name), c("Common.Name",
    "Scientific.Name", "Taxonomic.Resolution", "Unique.Taxonomic.Identification.Number")]
rownames(z3) <- z3$Scientific.Name
z3 <- z3[z3$Scientific.Name != "zero.pseudo",]
t31 <- system.time(s3 <- stcs(b3[,c("Label","Scientific.Name","Counts")]))
t32 <- system.time(m30 <- mefa(s3))
t33 <- system.time(m31 <- mefa(s3, x3, z3))
y30 <- m30$xtab
t34 <- system.time(m32 <- mefa(y30, x3, z3))

## S4 mefa4 processing
b4 <- b
b4$Label <- with(b4, paste(ABMI.Site, Year, Point.Count.Station, sep="_"))
x4 <- b4[!duplicated(b4$Label), c("Label", "ABMI.Site", "Year", "Field.Date", "Point.Count.Station",
    "Wind.Conditions", "Precipitation")]
rownames(x4) <- x4$Label
z4 <- b4[!duplicated(b4$Scientific.Name), c("Common.Name",
    "Scientific.Name", "Taxonomic.Resolution", "Unique.Taxonomic.Identification.Number")]
rownames(z4) <- z4$Scientific.Name
t41 <- system.time(s4 <- Xtab(~ Label + Scientific.Name, b4, cdrop = c("NONE", "SNI"), 
    subset = !(b4$Scientific.Name %in% c("VNA", "DNC", "PNA")), drop.unused.levels = TRUE))
t42 <- system.time(m40 <- Mefa(s4))
t43 <- system.time(m41 <- Mefa(s4, x4, z4))
y40 <- as.matrix(m40@xtab)
t44 <- system.time(m42 <- Mefa(y40, x4, z4))

res <- cbind("SIZE, *=3"=c("b*"=object.size(b3),
    "s*"=object.size(s3),
    "y*0"=object.size(y30),
    "m*0"=object.size(m30),
    "m*1"=object.size(m31),
    "m*2"=object.size(m32)),
"SIZE, *=4"=c("b*"=object.size(b4),
    "s*"=object.size(s4),
    "y*0"=object.size(y40),
    "m*0"=object.size(m40),
    "m*1"=object.size(m41),
    "m*2"=object.size(m42)),
"TIME, *=3"=c("b*"=NA,
    "s*"=t31[3],
    "y*0"=NA,
    "m*0"=t32[3],
    "m*1"=t33[3],
    "m*2"=t34[3]),
"TIME, *=4"=c("b*"=NA,
    "s*"=t41[3],
    "y*0"=NA,
    "m*0"=t42[3],
    "m*1"=t43[3],
    "m*2"=t44[3]))
(res <- cbind(res, "SIZE"=res[,2]/res[,1], "TIME"=res[,4]/res[,3]))

## debugging

dim(y30)
dim(y40)

setdiff(rownames(y30), rownames(y40))
setdiff(rownames(y40), rownames(y30))

setdiff(colnames(y30), colnames(y40))
setdiff(colnames(y40), colnames(y30))

## examples

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
coercion methods between S3 and S4 classes

