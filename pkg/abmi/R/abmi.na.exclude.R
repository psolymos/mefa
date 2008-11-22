abmi.na.exclude <-
function(x, value = NA, na.char=c("NA", "VNA", "DNC", "PNA", "SNI"),
drop = FALSE)
{
   if (NCOL(x) == 1) {
       i <- which(!is.na.abmi(x, na.char))
       j <- which(is.na.abmi(x, na.char))
   } else {
       i <- which(apply(is.na.abmi(x, na.char), 1, sum) == 0)
       j <- which(apply(is.na.abmi(x, na.char), 1, sum) != 0)
   }
   x <- x[i, drop = drop]
   attr(x, "na.action") <- j
   class(attr(x, "na.action")) <- "exclude"
   return(x)
}
