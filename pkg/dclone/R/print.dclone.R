print.dclone <-
function(x, ...)
{
    rval <- x
    nc <- nclones(x)
    attr(rval, "n.clones") <- NULL
    CLASS <- class(x)[!(class(x) %in% "dclone")]
    class(rval) <- CLASS
    cat("Object of class", CLASS, "with", nc, "clones\n")
    print(rval, ...)
}

