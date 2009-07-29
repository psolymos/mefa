dclone.default <-
function(x, n.clones=1, ...)
{
    if (is.list(x) && !is.data.frame(x))
        dclone.list(x, n.clones=n.clones, ...)
    if (n.clones==1) {
        rval <- x
    } else {
        if (!is.null(dim(x)) || is.list(x)) {
            NAMES <- row.names(x)
            rval <- lapply(as.data.frame(x), function(z) rep(z, n.clones))
            rval <- as.data.frame(rval)
            if (!is.null(NAMES))
                row.names(rval) <- paste(NAMES, rep(1:n.clones, each=length(NAMES)), sep="_")
            if (is.matrix(x)) {
                rval <- as.matrix(rval)
                colnames(rval) <- colnames(x)
            }
            if (is.list(x) && !is.data.frame(x)) {
                rval <- as.list(rval)
                attr(rval,"row.names") <- NULL
            }
        } else {
            NAMES <- names(x)
            rval <- rep(array(x), n.clones)
            if (!is.null(NAMES))
                names(rval) <- paste(NAMES, rep(1:n.clones, each=length(NAMES)), sep="_")
        }
    }
    attr(rval, "n.clones") <- n.clones
    rval
}

dclone.list <- function(x, n.clones=1, 
multiply=NULL, unchanged=NULL, ...)
{
    out <- lapply(x, dclone, n.clones=n.clones, ...)
    if (!is.null(multiply))
        out[multiply] <- lapply(x, "*", n.clones)[multiply]
    if (!is.null(unchanged))
        out[unchanged] <- x[unchanged]
    attr(out, "n.clones") <- n.clones
    out
}
