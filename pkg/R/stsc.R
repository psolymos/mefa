`stsc` <-
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

