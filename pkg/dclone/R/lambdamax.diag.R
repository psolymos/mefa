lambdamax.diag <-
function(x)
{
    max(eigen(var(report(x, array)), symmetric=TRUE, only.values=TRUE)$val)
}
