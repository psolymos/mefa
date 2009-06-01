nobservations.default <-
function (x, ...)
{
    if (!is.null(x$residuals)) {
        length(x$residuals)
    }
    else {
        if (!is.null(x$y)) {
            NROW(x$y)
        }
        else {
            x$n
        }
    }
}
