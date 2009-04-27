nobservations <-
function (fit, where=NULL) {
    if (is.null(where)) {
        if (!is.null(fit$residuals)) {
            length(fit$residuals)
        } else {
            if (!is.null(fit$y)) {
                NCOL(fit$y)
            } else {
                fit$n
            }
        }
    } else fit[[where]]
}
