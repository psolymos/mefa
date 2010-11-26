compile.abmi <- function(file, dir) {
    opb <- pboptions(type="tk")
    fnam <- file
    dir.in <- paste(dir, "/", "data", sep="")
    dir.out <- paste(dir, "/", "out", sep="")
    odir <- getwd()
    setwd(dir)
    unzip(fnam, exdir="data")
    dir.create(dir.out)
    ## get input file list
    files <- list.files(dir.in)
    files <- files[substr(files, nchar(files)-2, nchar(files)) == "csv"]
    Names <- sapply(strsplit(files, "_"), function(z) paste(z[-length(z)], collapse="_"))
    ##
    check <- c(" - OK\n", " - ERROR\n")
    ## use options
    options("abmi"=list(dir=dir, dir.in=dir.in, dir.out=dir.out, check=check,
        files=files, names=Names))

    ## start birds
    if("RAW_T26BreedingBirds" %in% Names)
        ## tris needs to be changed into system files...
        source("abmi-proto-birds.R")
#source(system.file(package = "abmi", "abmi-proto-birds.R"))

    ## how to keep track of what has been done?? high level log?
    ## also: keep track of things on screen with flush screen?

    ## what to return nere? what was looked for and what was done, etc
    setwd(odir)
    pboptions(opb)
#    options("abmi"=NULL)
    invisible(NULL)
}
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
read.abmi <- function(file, convert.na=TRUE, ...) {
    x <- read.csv(file, ...)
    cn <- colnames(x)
    cn <- sub("..", ".", cn, fixed = TRUE)
    id <- substr(cn, nchar(cn), nchar(cn)) != "."
    n <- ifelse(id, nchar(cn), nchar(cn)-1)
    cn <- substr(cn, 1, n)
    colnames(x) <- cn
    if (convert.na)
        x <- as.data.frame(lapply(x, na.abmi))
    x
}
## this is slow -- avoid
aggregate.abmi <- function(x, by, FUN, ...) {
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
