image.dist <-
function(x, grad, labels=FALSE, ...) {
    ## labels not yet used but could be done to add labels to left axis
    z <- as.matrix(x)
    if (!missing(grad))
        z <- z[order(grad), order(grad)]
    z[upper.tri(z)] <- diag(z)[1]
    pv <- t(1-z/max(z))
    image(pv, ylim=c(1,0), axes=FALSE, ...)
    invisible(pv)
}

