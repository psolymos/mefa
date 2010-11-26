abmi.na.exclude <-
function(x, value = NA, na.char=c("NA", "VNA", "DNC", "PNA", "SNI"))
{
   if (NCOL(x) == 1) {
       i <- which(!abmi.is.na(x, na.char))
       j <- which(abmi.is.na(x, na.char))
       x <- x[i]
   } else {
       i <- which(apply(abmi.is.na(x, na.char), 1, sum) == 0)
       j <- which(apply(abmi.is.na(x, na.char), 1, sum) != 0)
       x <- x[i,]
   }
   attr(x, "na.action") <- j
   class(attr(x, "na.action")) <- "exclude"
   return(x)
}
