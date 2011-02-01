cdrop <-
function(x, cutoff=0, attrib=FALSE)
{
    cs <- if (inherits(x, "mefa") || inherits(x, "Mefa"))
        colSums(xtab(x)) else colSums(x)
    if (any(cs <= cutoff)) {
        exclude <- which(cs <= cutoff)
        rval <- x[,-exclude]
        if (attrib) {
            attr(rval, "exclude") <- exclude
            attr(attr(rval, "exclude"), "cutoff") <- cutoff
            attr(attr(rval, "exclude"), "margin") <- 2
        }
        rval
    } else x
}
