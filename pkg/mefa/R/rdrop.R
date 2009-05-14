rdrop <-
function(x, cutoff=0)
{
    if (any(rowSums(x) <= cutoff)) {
        exclude <- which(rowSums(x) <= cutoff)
        rval <- x[-exclude,]
        attr(rval, "exclude") <- exclude
        attr(attr(rval, "exclude"), "cutoff") <- cutoff
        attr(attr(rval, "exclude"), "margin") <- 1
        rval
    } else x
}
