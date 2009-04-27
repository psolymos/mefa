nobservations <-
function (fit, where=NULL) {
    if (!is.null(where)) {
        if (!is.null(object$residuals)) {
            length(object$residuals)
        } else {
            if (!is.null(object$y)) {
                NCOL(fit$y)
            } else {
                fit$n
            }
        }
    } else fit[[where]]
}
