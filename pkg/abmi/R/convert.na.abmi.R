convert.na.abmi <-
function(x, value = NA, na.char=c("NA", "VNA", "DNC", "PNA", "SNI"))
{
   x[abmi.is.na(x)] <- value
   return(x)
}
