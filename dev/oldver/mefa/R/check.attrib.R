`check.attrib` <-
function (xc, which = c("samples", "species"), attrib, index = 0) 
{
    if (class(xc) != "xcount") 
        stop("Object '", xc, "' is not of 'xcount' class.")
    if (which == "species") {
        list.in.xc <- colnames(xc$data)
    }
    else {
        if (which == "samples") {
            list.in.xc <- rownames(xc$data)
        }
        else {
            stop("Undefined 'which' tag.")
        }
    }
if(index == 0) {list.in.attr <- rownames(attrib)} else {
    list.in.attr <- as.character(attrib[, index])
}
    if (length(union(list.in.xc, list.in.attr)) - nlevels(as.factor(list.in.xc)) - 
        nlevels(as.factor(list.in.attr)) == 0) {
        duplicate <- NULL
        set.relation <- "separate"
        missing <- list.in.xc
    }
    else {
        match <- subset(list.in.attr, is.element(list.in.attr, 
            list.in.xc))
        if (sum(apply(table(match, match) > 1, 1, sum)) == 0) 
            duplicate <- NULL
        else {
            duplicate <- subset(levels(as.factor(match)), apply(table(match, 
                match) > 1, 1, sum) != 0)
        }
        if (length(intersect(list.in.xc, list.in.attr)) == length(list.in.xc)) {
            missing <- NULL
            if (setequal(list.in.xc, list.in.attr) == TRUE) {
                set.relation <- "equal"
            }
            else {
                set.relation <- "inclusion"
            }
        }
        else {
            set.relation <- "intersect"
            missing <- setdiff(list.in.xc, list.in.attr)
        }
    }
    
    out <- list(
        call=match.call(), 
        set.relation = set.relation, 
        duplicate = duplicate, 
        missing = missing, 
        na = sum(is.na(attrib))
        )
    return(out)
}
