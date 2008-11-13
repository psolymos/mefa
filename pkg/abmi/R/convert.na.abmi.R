## converts all kind of ABMI NAs into R's NA
convert.na.abmi <-
function(x, value = NA, na.char=c("NA", "VNA", "DNC", "PNA", "SNI"))
{
    x[is.na.abmi(x)] <- value
    return(x)
}
