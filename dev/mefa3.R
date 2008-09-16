x <- fill.na(dol.count[,c(1,2,4,3)])
x[1,3] <- 0
m <- mefa(stsc(x,zero.pseudo="zero.count"),dol.sample,landsnail,1,2,segment=TRUE)
unclass(aggregate(m,by.samp,by.taxa))
aggregate(m[1:10,1:5], rep(1:2,5))

## Example

x <- data.frame(sample = LETTERS[c(1,1,2,2,3,4)],
    species = c(letters[c(5,5,5,6,7)], "zero.pseudo"),
    count = c(1,2,10,3,4,0), segment = letters[c(8,9,8,9,8,8)])

TODO: 2 dolina data set, MRI (GR,LF), UTM into landsnails

## xtab már rendezve van, ha mefa-n belülről hívom, de a végén tesztelni kell

## REPLACE THIS WITH THE MORNING VERSION
## taxo//taxa CHANGE IT!!!

check.tables <-
function(xtab, dframe, margin, index=NULL, drop.index=TRUE, xtab.fixed=TRUE)
{
    if (margin != 1 && margin != 2)
        stop("margin should be 1 or 2")
## itt margin szerint csak az egyik oldalt rendezni + rang vektort is, amit a végén visszaállítani = eredeti sorrend!!!
## pl. rank.orig <- 1:length(xnam)[rownames(xtab)]
    if (margin == 1) {
# ide rank.orig
        xtab <- xtab[order(rownames(xtab)), ]
        xnam <- rownames(xtab)
        } else {
# ide rank.orig
        xtab <- xtab[, order(colnames(xtab))]
        xnam <- colnames(xtab)}
    if (!is.null(index)) 
        rownames(dframe) <- dframe[, index]
    dnam <- rownames(dframe)
    if (xtab.fixed) {
        dsub <- dframe[dnam %in% xnam, ]
# itt rank.orig-gal rendezni
        dsub <- dsub[order(rownames(dsub)), ]
        xsub <- xtab
    } else { # xtab not fixed
        int <- intersect(dnam, xnam)
        dsub <- dframe[dnam %in% int, ]
# itt rank.orig-gal rendezni
        dsub <- dsub[order(rownames(dsub)), ]
        xsub <- if (margin == 1)
            xtab[xnam %in% int, ] else xtab[, xnam %in% int]
        }
    nsub <- if (margin == 1)
        rownames(xsub) else colnames(xsub)
    if (!identical(rownames(dsub), nsub))
        stop("tables do not match")
    if (!is.null(dsub) && drop.index)
        dsub[, index] <- NULL
    return(list(xtab=xsub, dtab=dsub))
}


## methods for mefa so far:
## print, [., aggregate, dim, dimnames, summary, plot, biplot, as., is.
## print.summary
## planned: report, melt

## helper fun mostly equivalent to xcount and xclist
## if segm=T & segment length = 1 returns NULL
## else matrix or list of matrices
## is nested=T, segment matrices are added 1, 1+2, 1+2+3 etc
## col & row names a table() miatt rendezve vannak
crosstab <-
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

## IF IMPLEMENTING fixed=c("xtab","dframe","intersect")
## search for possible places where arg occurs: check.tables, "$<-.mefa"
mefa <-
function(xtab, samp=NULL, taxa=NULL, samp.id=NULL, taxa.id=NULL, segment=FALSE, nested=FALSE,
drop.zero=FALSE, drop.index=TRUE, xtab.fixed=TRUE)
{
    x <- xtab
    if (min(dim(as.matrix(x))) == 1)
        stop("xtab should not be vector or list")
    if (any(is.na(x)))
        stop("xtab contains NA")
    if (nested && !segment)
        warning("nested = TRUE has no effect if segment = FALSE")
    if (!is.stsc(x)) {
        xtab <- as.matrix(x)
        segm <- NULL}

    if (is.stsc(x)) {
        xtab <- crosstab(x, segment=FALSE, drop.zero=drop.zero)
        segm <- if (segment) {
            crosstab(x, segment=TRUE, nested=nested, drop.zero=drop.zero)
            } else {NULL}}

## ide jön a samp & taxa leválogatás és sorba rendezés, jó lenne ha az xtab sor & oszlopnevek maradnának
## xtab.fixed=TRUE, xtab sor/oszlop nevei fixek, ha F akkor a legszűkebb halmazt adja eredményül
    xtab2 <- xtab
    if (!is.null(samp)) {
        samp.list <- check.tables(xtab2, samp, 1, samp.id, drop.index=drop.index, xtab.fixed=xtab.fixed)
        xtab2 <- samp.list$xtab
        samp2 <- samp.list$dtab
        } else {
        samp2 <- NULL}
    if (!is.null(taxa)) {
        taxa.list <- check.tables(xtab2, taxa, 2, taxa.id, drop.index=drop.index, xtab.fixed=xtab.fixed)
        xtab2 <- taxa.list$xtab
        taxa2 <- taxa.list$dtab
        } else {
        taxa2 <- NULL}

## segm list subsetting xtab2 alapján
    if (segment && !is.null(segm) && !xtab.fixed) {
        row.sub <- which(rownames(segm[[1]]) %in% rownames(xtab2))
        col.sub <- which(colnames(segm[[1]]) %in% colnames(xtab2))
        for (i in 1:length(segm)) {
            segm[[i]] <- segm[[i]][row.sub, col.sub]}}

    out <- list(call = match.call(), xtab = xtab2, segm = segm, samp = samp2, taxa = taxa2)
    class(out) <- c("mefa", "list")
    attr(out, "nested") <- nested
    attr(out, "drop.zero") <- drop.zero
    attr(out, "xtab.fixed") <- xtab.fixed
    return(out)
}

print.mefa <-
function(x, ...)
{
#cat("\nCall: ", deparse(x$call), "\n", sep="")
cat("\nAn object of class \"mefa\" containing\n\n", sep="")
indiv <- if (sum(x$xtab) == 1) " individual" else " individuals"
cat(" - ", sum(x$xtab), indiv, " of ", ncol(x$xtab), " taxa in ", nrow(x$xtab), " samples,\n", sep="")
cat(" - ", sep="")
if (!is.null(x$segm)) {
    cat(length(x$segm), sep="")
    if (attr(x, "nested")) cat(" nested", sep="") else cat(" (non nested) segments,\n", sep="")
    } else cat("1 (all inclusive) segment,\n", sep="")
prov <- if (!is.null(x$samp)) "provided,\n" else "not provided,\n"
cat(" - table for samples ", prov, sep="")
prov <- if (!is.null(x$taxa)) "provided." else "not provided."
cat(" - table for taxa ", prov, sep="")
cat("\n\n")
invisible(x)}

summary.mefa <-
function(object, ...)
{
x <- object
out <- list(
    srich = rowSums(x$xtab > 0),
    ninds = rowSums(x$xtab),
    spocc = colSums(x$xtab),
    spabu = colSums(x$xtab > 0),
    nsamp = dim(x)[1],
    ntaxa = dim(x)[2],
    nsegm = dim(x)[3],
    ntot = sum(x$xtab),
    mfill = sum(x$xtab > 0),
    segment = dimnames(x)$segm,
    call = x$call,
    nested = attr(x, "nested"),
    drop.zero = attr(x, "drop.zero"),
    xtab.fixed = attr(x, "xtab.fixed"))
class(out) <- c("summary.mefa", "list")
return(out)}

## only FUN=sum is available, ecause this results in count as well
aggregate.mefa <-
function(x, by.samp=NULL, by.taxa=NULL)
{
    if (is.null(by.samp) && is.null(by.taxa))
        return(x)
    xtab <- as.data.frame(x$xtab)
    if (!is.null(by.samp)) {
        x$samp <- NULL
        xtab <- aggregate(xtab, list(by.samp), sum)
        rownames(xtab) <- xtab[,1]
        xtab[,1] <- NULL
        if (!is.null(x$segm)){
            for (i in 1:length(x$segm)) {
                x$segm[[i]] <- aggregate(x$segm[[i]], list(by.samp), sum)
                rownames(x$segm[[i]]) <- x$segm[[i]][,1]
                x$segm[[i]][,1] <- NULL}}
        }
    if (!is.null(by.taxa)) {
        x$taxa <- NULL
        xtab <- aggregate(t(xtab), list(by.taxa), sum)
        rownames(xtab) <- xtab[,1]
        xtab[,1] <- NULL
        xtab <- t(xtab)
        if (!is.null(x$segm)){
            for (i in 1:length(x$segm)) {
                x$segm[[i]] <- aggregate(t(x$segm[[i]]), list(by.taxa), sum)
                rownames(x$segm[[i]]) <- x$segm[[i]][,1]
                x$segm[[i]][,1] <- NULL
                x$segm[[i]] <- t(x$segm[[i]])}}
        }
    x$xtab <- as.matrix(xtab)
    x$call <- match.call()
    return(x)
}


dim.mefa <-
function (x) 
{
    nseg <- if (is.null(x$segm)) 1 else length(x$segm)
    out <- c(nrow(x$xtab), ncol(x$xtab), nseg)
    return(out)
}


"dimnames.mefa" <-
function (x)
{
  list(samp = rownames(x$xtab),
       taxa = colnames(x$xtab),
       segm = names(x$segm))
}



## does it make sense to have 3 dimensions???
## if k!=1:dim(x)[3] akkor x$xtab-ot is újra kell számoltatni
"[.mefa" <-
function (x, i=1:dim(x)[1], j=1:dim(x)[2], k=1:dim(x)[3])
{
    if (length(i) == 0 || length(j) == 0 || length(k) == 0)
        return(NULL)
    if (length(i) == 1 || length(j) == 1) {
        warning("dimensions are too few to be extracted")
        return(x)}
    subsegm <- if (!is.null(x$segm))
        length(k) != length(x$segm) else FALSE
    x$call <- match.call()

    if (is.null(x$segm)) {
        x$segm <- NULL} else {
        segm.out <- list()
        for (nn in 1:length(k))
            segm.out[[nn]] <- x$segm[[k[nn]]][i, j]
        names(segm.out) <- names(x$segm)[k]
        x$segm <- segm.out}

    if (!subsegm) xtab.out <- x$xtab[i, j]
        else {
        if (!is.null(x$segm) && length(k) == 1)
            xtab.out <- segm.out[[1]] else {
            xtab.out <- segm.out[[1]]
            for (nn in 2:length(k))
                xtab.out <- xtab.out + segm.out[[nn]] }}
    x$xtab <- xtab.out
    x$samp <- if (is.null(x$samp))
        NULL else x$samp[i, ]
    x$taxa <- if (is.null(x$taxa))
        NULL else x$taxa[j, ]
    return(x)
}



print.summary.mefa <-
function(x, ...)
{
    print(unclass(x))
}

report <-
function (x,...)
{
    UseMethod("report")
}
report.default <- print
report.mefa <-
function (x, ...)
{
    cat("report method not yet implemented\n")
    invisible()
}

melt <-
function (x,...)
{
    UseMethod("melt")
}
melt.default <- unlist
melt.mefa <-
function (x, ...)
{
    cat("melt method not yet implemented\n")
    invisible()
}

plot.mefa <-
function(x, which=1:4, ylab=NULL, xlab=NULL, ...)
{
    if (!all(which %in% 1:4))
        stop("which must be in 1:4")
    if (!length(which) == 1) which <- 1
    if (which == 1) {
        if (is.null(ylab)) ylab <- "Frequency (samples)"
        if (is.null(xlab)) xlab <- "Number of taxa"
        plot(table(summary(x)$srich), xlab=xlab, ylab=ylab, ...)}
    if (which == 2) {
        if (is.null(ylab)) ylab <- "Frequency (samples)"
        if (is.null(xlab)) xlab <- "Number of individuals"
        plot(table(summary(x)$srich), xlab=xlab, ylab=ylab, ...)}
    if (which == 3) {
        if (is.null(ylab)) ylab <- "Frequency (taxa)"
        if (is.null(xlab)) xlab <- "Occupancy"
        plot(table(summary(x)$spocc), xlab=xlab, ylab=ylab, ...)}
    if (which == 4) {
        if (is.null(ylab)) ylab <- "Frequency (taxa)"
        if (is.null(xlab)) xlab <- "Abundance"
        plot(table(summary(x)$spocc), xlab=xlab, ylab=ylab, ...)}
    invisible()
}

boxplot.mefa <-
function(x, which=1:4, ylab=NULL, xlab=NULL, ...)
{
    if (is.null(x$segm) || dim(x)[3] == 1)
        stop("at least 2 segments needed")
    if (!all(which %in% 1:4))
        stop("which must be in 1:4")
    if (!length(which) == 1) which <- 1
    yval <- list()
    for (i in 1:dim(x)[3])
        yval[[i]] <- summary(mefa(m$segm[[i]]))[[which]]
    yval <- unlist(yval)
    xval <- as.factor(rep(dimnames(x)$segm, each=length(summary(x)[[which]])))
    if (is.null(ylab))
        ylab <- c("Frequency (samples)", "Frequency (samples)", "Frequency (taxa)", "Frequency (taxa)")[which]
    if (is.null(xlab))
        xlab <- c("Number of taxa", "Number of individuals", "Occupancy", "Abundance")[which]
    plot(yval ~ xval, xlab=xlab, ylab=ylab, ...)
    invisible()
}

is.mefa <-
function(x)
{
    if (!inherits(x, "mefa"))
        return(FALSE)
    cr1 <- is.matrix(x$xtab)
    cr2 <- if (is.null(x$segm)) TRUE else is.list(x$segm)
    cr3 <- if (is.null(x$samp)) TRUE else is.data.frame(x$samp)
    cr4 <- if (is.null(x$taxa)) TRUE else is.data.frame(x$taxa)
    cr5 <- if (is.null(x$segm)) TRUE
        else sum(x$xtab) == sum(unlist(x$segm))
    length(x) == 5 && cr1 && cr2 && cr3 && cr4 && cr5
}# - check if sum(x$segm) == sum(x$xtab)


as.mefa <-
function(x, ...)
{
    if (inherits(x, "mefa"))
        return(x) else return(mefa(x, ...))
}

## This replaces the old 'sscount' function
## is.stsc and as.stsc S3 methods provided
## inflate vectorized
## fill.na is same as fill.count (but there were no count at all)

stsc <-
function(xtab, expand = FALSE, drop.zero = FALSE, zero.pseudo="zero.pseudo")
{
    x <- xtab
    if (any(is.na(x)))
        stop("xtab contains NA")
    if (min(dim(as.matrix(x))) == 1)
        stop("2-4 columns required")
    nows <- nrow(x)
    ncols <- ncol(x)
    if (ncols > 4) stop("2-4 columns required")
    x <- data.frame(x)
    if (ncols == 2)
        x <- data.frame(x, rep(1, nrows), rep("all", nrows))
    if (ncols == 3)
        x <- data.frame(x, rep("all", nrows))
    if (!is.numeric(x[, 3]))
        stop("count column must be numeric")
    if (any(x[, 3] == 0)) {
        zpart <- x[x[, 3] == 0,]
        x <- x[x[, 3] != 0,]
        } else {
        zpart <- NULL}
    if (drop.zero && !is.null(zpart))
        zpart <- NULL
    if (expand) {
        tmp <- data.frame(inflate(x[, c(1:2,4)], x[, 3]))
        x <- data.frame(tmp[, 1:2], rep(1, sum(x[, 3])), tmp[, 3])}
    if (!is.null(zpart)){
        if (zero.pseudo %in% unique(as.character(x[,2 ])))
            stop("\"zero.pseudo\" found in taxa names: specify zero.pseudo")
        zpart[,2] <- rep(zero.pseudo, nrow(zpart))
        zpart[,4] <- rep(zero.pseudo, nrow(zpart))
        joint <- which(unique(zpart[,1]) %in% unique(x[,1]))
        if (length(joint) != 0) {
            zpart <- zpart[-joint,]
            if (nrow(zpart) == 0)
                zpart <- NULL
            warning("zero count for non zero sample found")}
            }
    if (!is.null(zpart)){
        colnames(x) <- 1:4
        colnames(zpart) <- 1:4
        x <- merge(x, zpart, all = TRUE)
        } else zero.pseudo <- NULL
    rownames(x) <- NULL
    colnames(x) <- c("samp", "taxa", "count", "segm")
    x$samp <- as.factor(x$samp)
    x$taxa <- as.factor(x$taxa)
    x$segm <- as.factor(x$segm)
    x[] <- lapply(x, function(x) x[drop = TRUE])
    class(x) <- c("stsc", "data.frame")
    attr(x, "expand") <- expand
    attr(x, "zero.count") <- any(x$count == 0)
    attr(x, "zero.pseudo") <- zero.pseudo
    return(x)
}

as.stsc <-
function(x, ...) stsc(x, ...)

is.stsc <-
function(x) {
    inherits(x, "stsc") && colnames(x) == c("samp", "taxa", "count", "segm") && is.numeric(x[, 3]) &&
    is.factor(x$samp) && is.factor(x$taxa) && is.factor(x$segm)}


## x can be anything (vector, matrix, data.frame)
## value is matrix sum(y) rows, ncol(x) cols
inflate <-
function (x, y)
{
    x <- data.frame(x)
    if (nrow(x) != length(y))
        stop("dimensions do not match")
    if (!identical(all.equal(y, round(y)), TRUE))
        stop("y must be integer")
    if (any(y <= 0))
        stop("y must be positive")
    out <- apply(x, 2, function(x) {
        z <- mapply(rep, x, y)
        names(z) <- NULL # this is needed to overcome dupl. row names
        unlist(z)
        })
    return(out)
}

fill.na <-
function (x)
{
    out <- x
    for (cols in 1:ncol(x)) {
        for (rows in 1:nrow(x)) {
            if (is.na(x[rows, cols])) {
                rows2 <- rows - 1
                out[rows, cols] <- out[rows2, cols]
            }
        }
    }
    return(out)
}

