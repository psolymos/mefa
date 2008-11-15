# Omits ABMI NA values
abmi.na.omit <-
function(x, value = NA, na.char=c("NA", "VNA", "DNC", "PNA", "SNI"), drop = FALSE)
{
    if (NCOL(x) == 1) return(x[!abmi.is.na(x), drop = drop])
    else return(x[apply(abmi.is.na(x), 1, sum) == 0, drop = drop])
}
