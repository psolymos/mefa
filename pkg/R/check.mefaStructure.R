`check.mefaStructure` <-
function(x)
{
    if (!inherits(x, "mefa"))
        stop("object is not of class \"mefa\"")

    n <- 10
    mt <- rep(TRUE, n)
    ret <- character(n)

    mt[1] <- length(x) == 5
    mt[2] <- is.matrix(x$xtab)
    if (is.null(x$segm)) {
        mt[3] <- is.list(x$segm)
        mt[4] <- sum(x$xtab) == sum(unlist(x$segm))
        for (i in 1:dim(x)[3]) {
            mt[5] <- identical(rownames(x$xtab), rownames(x$segm[[i]]))
            mt[6] <- identical(colnames(x$xtab), colnames(x$segm[[i]]))
            }
        }
    if (is.null(x$samp)) {
        mt[7] <- is.data.frame(x$samp)
        mt[8] <- identical(rownames(x$xtab), rownames(x$samp))
        }
    if (is.null(x$taxa)) {
        mt[9] <- is.data.frame(x$taxa)
        mt[10] <- identical(colnames(x$xtab), rownames(x$taxa))
        }

    if (!mt[1]) ret[1] <- "object length is not 5\n"
    if (!mt[2]) ret[2] <- "$xtab is not matrix\n"
    if (!mt[3]) ret[3] <- "$segm is not list\n"
    if (!mt[4]) ret[4] <- "sum of $xtab and sum of $segm not equal\n"
    if (!mt[5]) ret[5] <- "rownames in $xtab and $segm not identical\n"
    if (!mt[6]) ret[6] <- "colnames in $xtab and $segm not identical\n"
    if (!mt[7]) ret[7] <- "$samp is not data.frame\n"
    if (!mt[8]) ret[8] <- "rownames in $xtab and $samp not identical\n"
    if (!mt[9]) ret[9] <- "$taxa is not data.frame\n"
    if (!mt[10]) ret[10] <- "colnames in $xtab and rownames in $taxa not identical\n"

    if (all(mt == TRUE))
        return(NULL) else return(print(paste(ret, sep="")))
}

