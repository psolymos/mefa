`compress.default` <-
function (x, y, ...)
{
    if (is.numeric(x)) {
        return(as.numeric(compress(as.factor(x), y)))
    } else {
        return(as.character(compress(as.factor(x), y)))}
}

