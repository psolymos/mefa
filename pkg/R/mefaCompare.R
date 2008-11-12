`mefaCompare` <-
function(x1, x2)
{
    if (!inherits(x1, "mefa") || !inherits(x2, "mefa"))
        stop("compared objects must be of class 'mefa'")
    if (!identical(dim(x1), dim(x2)))
        stop("dimensions differ")

    k <- dim(x1)[3]
    xt1 <- xt2 <- list()
    xt1[[1]] <- x1$xtab[order(rownames(x1$xtab)), order(colnames(x1$xtab))]
    xt2[[1]] <- x2$xtab[order(rownames(x2$xtab)), order(colnames(x2$xtab))]
    sego <- match(dimnames(x1)$segm, dimnames(x2)$segm)

    if (k > 1)
        for (i in 1:k) {
            xt1[[(i + 1)]] <- x1$segm[[i]][order(rownames(x1$segm[[i]])),
                order(colnames(x1$segm[[i]]))]
            xt2[[(i + 1)]] <- x2$segm[[sego[i]]][order(rownames(x2$segm[[sego[i]]])),
                order(colnames(x2$segm[[sego[i]]]))]
        }
    j <- length(xt1)
    rv <- logical(3 * j + 1)
    for (i in 1:j) {
        rv[i] <- all(xt1[[i]] == xt2[[i]])
        rv[(i + j)] <- all(rownames(xt1[[i]]) == rownames(xt2[[i]]))
        rv[(i + 2*j)] <- all(colnames(xt1[[i]]) == colnames(xt2[[i]]))
    }
    rv[(3 * j + 1)] <- all(dimnames(x1)[[3]] %in% dimnames(x2)[[3]])
    return(all(rv))
}

