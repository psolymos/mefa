BIC.default <-
function(object, ...) {
    rval <- AIC(object, k=log(nobservations(fit)), ...)
    colnames(rval)[2] <- "BIC"
    Call <- match.call()
    rownames(rval) <- as.character(Call[-1])
    rval
}
