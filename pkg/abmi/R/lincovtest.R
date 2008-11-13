## tests linear relationship for binomial GLM
lincovtest <-
function(y, x, probs = seq(0, 1, 0.25), na.rm = FALSE, ...)
{
    require(mefa)
    q <- qvector(x, probs = seq(0, 1, 0.25), na.rm = na.rm, ...)
    z <- as.factor(q)
    m <- coef(glm(y ~ z, family=binomial))
    names(m) <- levels(z)
    class(m) <- c("lincovtest", class(m))
    return(m)
}
