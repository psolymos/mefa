glmexplorer <-
function(formula, data, family=poisson(), ...)
{
    lhs <- formula[[2]]
    if (missing(data))
        data <- parent.frame()
    Y <- as.matrix(eval(lhs, data))
    formula[[2]] <- NULL
    if (!identical(formula[[2]], 1)) {
        rhs <- model.frame(formula, data, drop.unused.levels = TRUE)
        X <- model.matrix(terms(formula), rhs)
    } else {
        X <- matrix(1, nrow(Y), 1)
    }
    if (nrow(Y) != nrow(X))
        stop("number of rows in Y and X must conform")
    if (is.na(Y) || is.na(X))
        stop("'NA' values not allowed")
    n <- NROW(Y)
    J <- NCOL(Y)
    glmfit <- lapply(1:J, function(z) glm.fit(X, Y[, z], family=family, ...))
    glmsum <- lapply(glmfit, function(z) {
        class(z) <- "glm"
        summary(z)$coef})
    names(glmsum) <- colnames(Y)
    class(glmsum) <- "glmexplorer"
    glmsum
}

