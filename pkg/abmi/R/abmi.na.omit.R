# Omits ABMI NA values
abmi.na.omit <-
function(x, value = NA, na.char=c("NA", "VNA", "DNC", "PNA", "SNI"), drop = FALSE)
{
    if (NCOL(x) == 1) return(x[!is.na.abmi(x), drop = drop])
    else return(x[apply(is.na.abmi(x), 1, sum) == 0, drop = drop])
}
