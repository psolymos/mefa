`compress.factor` <-
function(x, y)
{
i <- interaction(x,y)
if (length(unique(i)) != length(unique(y)))
    stop("y should be nested in x")
out <- as.factor(aggregate(as.numeric(x), list(y), mean)[,2])
levels(out) <- levels(x)
return(out)
}


