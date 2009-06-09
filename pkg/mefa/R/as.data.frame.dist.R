as.data.frame.dist <-
function (x, row.names = NULL, optional = FALSE, ...)
{
    if (!missing(optional)) 
        .NotYetUsed("optional", error = FALSE)
    id <- as.matrix(x)
    id[lower.tri(id)] <- 1
    id[upper.tri(id)] <- 0
    diag(id) <- 0
    rm <- row(id)
    cm <- col(id)
    rm <- array(rm)[array(id) == 1]
    cm <- array(cm)[array(id) == 1]
    out <- data.frame(row=rm, col=cm, dist=dist2vec(x))
    if (!is.null(row.names))
        rownames(out) <- row.names
    out
}
