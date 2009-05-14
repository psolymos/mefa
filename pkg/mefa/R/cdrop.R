cdrop <-
function(x, cutoff=0)
{
    if (any(colSums(x) <= cutoff)) {
        exclude <- which(colSums(x) <= cutoff)
        rval <- x[,-exclude]
        attr(rval, "exclude") <- exclude
        attr(attr(rval, "exclude"), "cutoff") <- cutoff
        attr(attr(rval, "exclude"), "margin") <- 2
        rval
    } else x
}
