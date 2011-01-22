`as.array` <- function (x, ...)
    UseMethod("as.array")

`as.matrix.mefa` <-
function(x, ...)
{
    x$xtab
}

