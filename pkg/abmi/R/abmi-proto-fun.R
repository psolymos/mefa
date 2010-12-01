na.abmi <- function(x) {
    if (is.numeric(x))
        return(x)
    #VNA—Variable Not Applicable
    #DNC—Did Not Collect
    #PNA—Protocol Not Available
    #SNI—Species Not Identified
    vna <- which(x == "VNA")
    dnc <- which(x == "DNC")
    pna <- which(x == "PNA")
    sni <- which(x == "SNI")
    to.na <- unique(c(vna, dnc, pna, sni))
    x[to.na] <- NA
    x <- x[drop=TRUE]
    att <- list()
    att$vna <- if (identical(vna,integer(0)))
        NULL else vna
    att$dnc <- if (identical(dnc,integer(0)))
        NULL else dnc
    att$pna <- if (identical(pna,integer(0)))
        NULL else pna
    att$sni <- if (identical(sni,integer(0)))
        NULL else sni
    if (length(att) > 0)
        attr(x, "na.abmi") <- att
    x
}
read.abmi <- function(file, convert.na=c("pre","post","none"), ...) {
    convert.na <- match.arg(convert.na)
    x <- if (convert.na == "pre") {
        read.csv(file, na.strings=c("VNA","DNC","PNA","SNI"), ...)
    } else read.csv(file, ...)
    cn <- colnames(x)
    cn <- sub("..", ".", cn, fixed = TRUE)
    id <- substr(cn, nchar(cn), nchar(cn)) != "."
    n <- ifelse(id, nchar(cn), nchar(cn)-1)
    cn <- substr(cn, 1, n)
    colnames(x) <- cn
    if (convert.na == "post")
        x <- as.data.frame(lapply(x, na.abmi))
    x
}
## this is slow -- avoid
aggregate.abmi <- function(x, by, FUN, sort=NULL, ...) {
    if (is.numeric(x)) {
        z <- aggregate(x, list(by), FUN, ..., simplify=FALSE)
        out <- as.numeric(z[,2])
        names(out) <- as.character(z[,1])
    } else {
            v <- unique(as.character(by))
            x <- as.character(x)
        if (FUN=="first") {
            out <- pbsapply(v, function(z) x[by==z][1])
        }
        if (FUN=="freq") {
            tmp <- pblapply(v, function(z) table(x[by==z]))
            out <- pbsapply(tmp, function(z) {
                if (length(z) > 0)
                    names(z)[which.max(z)[1]] else NA
            })
        }
        names(out) <- v
    }
    if (!is.null(sort))
        out <- out[match(sort, names(out))]
    out
}
keepfirst <- function(x, id) {
    keep <- if (require(pbapply)) {
        pbsapply(unique(as.character(id)), function(z) which(id==z)[1])
    } else {
        sapply(unique(as.character(id)), function(z) which(id==z)[1])
    }
    x[keep,]
}
cat2 <- function(text, ..., file=NULL, print=TRUE, sep=" ") {
    if (!is.null(file))
        cat(text, ..., file = file, sep = sep)
    if (print) {
        cat(text, ..., sep = sep)
        flush.console()
    }
    invisible()
}
