`add.attrib` <-
function (mf, which = c("samples", "species"), attrib, index = 0) 
{
    if (class(mf) != "mefa") 
        stop("Object '", mf, "' is not of 'mefa' class.")
    xord <- xorder(as.xcount(mf$data), which, attrib, index)
    if (index != 0) 
        xord$data[[index]] <- NULL
    if (which == "species") {
        xsamp.out <- mf$sample.attr
        if(is.null(mf$species.attr)) {xspec.out <- as.data.frame(xord$data)
            } else {
            xspec.out <- data.frame(mf$species.attr, xord$data)}
    }
    if (which == "samples") {
        xspec.out <- mf$species.attr
        if(is.null(mf$sample.attr)) {xsamp.out <- as.data.frame(xord$data)
            } else {
            xsamp.out <- data.frame(mf$sample.attr, xord$data)}
    }
    xco <- as.xcount(mf$data)
    if(is.null(xsamp.out)) xo1 <- NULL else xo1 <- xorder(xco, "samples", xsamp.out)
    if(is.null(xspec.out)) xo2 <- NULL else xo2 <- xorder(xco, "species", xspec.out)
    out <- mefa(xco, xo1, xo2)
    return(out)
}
