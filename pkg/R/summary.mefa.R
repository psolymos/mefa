`summary.mefa` <-
function(object, ...)
{
x <- object
out <- list(
    srich = rowSums(x$xtab > 0),
    ninds = rowSums(x$xtab),
    spocc = colSums(x$xtab > 0),
    spabu = colSums(x$xtab),
    ntot = sum(x$xtab),
    mfill = sum(x$xtab > 0)/(dim(x)[1]*dim(x)[2]),
    nsamp = dim(x)[1],
    ntaxa = dim(x)[2],
    nsegm = dim(x)[3],
    segment = dimnames(x)$segm,
    call = x$call,
    nested = attr(x, "nested"),
    drop.zero = attr(x, "drop.zero"),
    xtab.fixed = attr(x, "xtab.fixed"))
class(out) <- c("summary.mefa")
return(out)}

