`melt.default` <-
function (x, recursive = TRUE, use.names = TRUE) 
{
    if (.Internal(islistfactor(x, recursive))) {
        lv <- unique(.Internal(unlist(lapply(x, levels), recursive, 
            FALSE)))
        nm <- if (use.names) 
            names(.Internal(unlist(x, recursive, use.names)))
        res <- .Internal(unlist(lapply(x, as.character), recursive, 
            FALSE))
        res <- match(res, lv)
        structure(res, levels = lv, names = nm, class = "factor")
    }
    else .Internal(unlist(x, recursive, use.names))
}
