rdrop <-
function(x, cutoff=0, attrib=FALSE)
{
    rs <- if (inherits(x, "mefa") || inherits(x, "Mefa"))
        rowSums(xtab(x)) else rowSums(x)
    if (any(rs <= cutoff)) {
        exclude <- which(rs <= cutoff)
        rval <- x[-exclude,]
        if (attrib) {
            attr(rval, "exclude") <- exclude
            attr(attr(rval, "exclude"), "cutoff") <- cutoff
            attr(attr(rval, "exclude"), "margin") <- 1
        }
        rval
    } else x
}
