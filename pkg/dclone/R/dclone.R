dclone <-
function(x, n.clones=1)
{
    if (n.clones==1) {
        rval <- x
    } else {
        if (!is.null(dim(x)) || is.list(x)) {
            rval <- lapply(as.data.frame(x), function(z) rep(z, n.clones))
            rval <- as.data.frame(rval)
            if (is.matrix(x))
                rval <- as.matrix(rval)
        } else {
            rval <- rep(array(x), n.clones)
        }
    }
    class(rval) <- c("dclone", class(x))
    attr(rval, "n.clones") <- n.clones
    rval
}

