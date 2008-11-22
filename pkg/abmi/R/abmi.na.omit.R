abmi.na.omit <-
function(x, value = NA, na.char=c("NA", "VNA", "DNC", "PNA", "SNI"))
{
   x <- abmi.na.exclude(x, value, na.char)
   class(attr(x, "na.action")) <- "omit"
   return(x)
}
