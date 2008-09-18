`dimnames.mefa` <-
function (x)
{
  list(samp = rownames(x$xtab),
       taxa = colnames(x$xtab),
       segm = names(x$segm))
}

