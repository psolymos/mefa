abmi.is.na <-
function(x, na.char=c("NA", "VNA", "DNC", "PNA", "SNI"))
{
   if (!is.data.frame(x) && NCOL(x) == 1) y <- I(x %in% na.char)
       else {
           is.it.mat <- is.matrix(x)
           y <- sapply(as.data.frame(x), function(a) I(a %in% na.char))
           if (is.it.mat) y <- as.matrix(y)
       }
   if (!is.null(dimnames(x)))
       dimnames(y) <- dimnames(x)
       else names(y) <- names(x)
   return(y)
}
