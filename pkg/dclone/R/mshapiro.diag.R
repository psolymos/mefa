mshapiro.diag <-
function(x)
{
    nch <- nchain(x)
    n <- niter(x)
    mcp <- mcpar(x[[1]])
    vmax <- 5000
    allch <- if (nch * n > vmax) {
        report(window(x, mcp[1] + n - trunc(vmax / nch), mcp[2]), array)
    } else report(x, array)
    mshapiro.test(t(allch))
}
