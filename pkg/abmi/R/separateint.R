## separates interaction vector
separateint <-
function(x, step = 1, spacer = 1)
{
    if (is.list(x))
        stop("'x' must not be list")
    x <- as.character(x)
    n <- nchar(x)
    step <- step - 1
    first <- substr(x, 1, n-step-1-spacer)
    second <- substr(x, n-step, n)
    out <- cbind(first, second)
    colnames(out) <- c("first", "second")
    rownames(out) <- x
    return(out)
}
