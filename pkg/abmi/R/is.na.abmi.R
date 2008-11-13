## returns logical object with same dimensions, TRUE where na.char matched
is.na.abmi <-
function(x, na.char=c("NA", "VNA", "DNC", "PNA", "SNI"))
{
    if (NCOL(x) == 1) y <- x %in% na.char
    else {
        is.it.mat <- is.matrix(x)
        y <- sapply(as.data.frame(x), function(a) a %in% na.char)
        if (is.it.mat) y <- as.matrix(y)
    }
    return(y)
}
