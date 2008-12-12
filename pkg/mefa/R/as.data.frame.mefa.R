as.data.frame.mefa <-
function(x, arrange, name, make.unique = FALSE)
{
    if (!inherits(x, "mefa"))
        stop("'x' must be of class mefa")
    if (missing(arrange))
        stop("'arrange' argument is missing")
    if (!is.function(arrange))
        stop("'arrange' argument must be a function")
    arrange(x, name)
}
