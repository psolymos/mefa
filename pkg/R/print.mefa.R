`print.mefa` <-
function(x, ...)
{
#cat("\nCall: ", deparse(x$call), "\n", sep="")
cat("\nAn object of class 'mefa' containing\n\n", sep="")
indiv <- if (sum(x$xtab) == 1) " individual" else " individuals"
cat(" - ", sum(x$xtab), indiv, " of ", ncol(x$xtab), " taxa in ", nrow(x$xtab), " samples,\n", sep="")
cat(" - ", sep="")

if (!is.null(x$segm)) {
    cat(length(x$segm), sep="")
    if (attr(x, "nested")) cat(" nested segments,\n", sep="") else cat(" (non nested) segments,\n", sep="")
    } else cat("1 (all inclusive) segment,\n", sep="")

prov <- if (is.null(x$samp)) "not provided,\n" else {
    paste("provided (", ncol(x$samp), " variables),\n", sep="")}
cat(" - table for samples ", prov, sep="")
prov <- if (is.null(x$taxa)) "not provided." else {
    paste("provided (", ncol(x$taxa), " variables).", sep="")}
cat(" - table for taxa ", prov, sep="")
cat("\n\n")
invisible(x)}

