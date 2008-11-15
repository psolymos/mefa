## converts all kind of ABMI NAs into R's NA
abmi.convert.na <-
function(x, value = NA, na.char=c("NA", "VNA", "DNC", "PNA", "SNI"))
{
    x[abmi.is.na(x)] <- value
    return(x)
}
