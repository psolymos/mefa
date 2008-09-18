`xorder` <-
function (xc, which = c("samples", "species"), attrib, index = 0) 
{
    if (class(xc) != "xcount") 
        stop("Object '", xc, "' is not of 'xcount' class.")
    check1 <- check.attrib(xc, which, attrib, index)$set.relation
    check2 <- check.attrib(xc, which, attrib, index)$duplicate
    if (sum(is.element(c("equal", "inclusion"), check1)) == 0) 
        stop("Set relation in 'check.attrib' result: ", check1, 
            ".\nMissing elements: ", check.attrib(xc, which, 
                attrib, index)$missing)

if(index == 0) {attr.i <- rownames(attrib)} else {attr.i <- attrib[, index]}

    if (!is.null(check2)) 
        stop("Duplicates were found by 'check.attrib': ", check2, 
            ".")
    if (check1 == "equal") {
        attrib.out <- attrib[order(as.character(attr.i)), 
            ]
        if (which == "species") 
            index.out <- as.character(colnames(xc$data))
        if (which == "samples") 
            index.out <- as.character(rownames(xc$data))
    }
    if (check1 == "inclusion") {
        if (which == "species") {
            exclude <- is.element(attr.i, colnames(xc$data))
            index.out <- as.character(colnames(xc$data))
        }
        if (which == "samples") {
            exclude <- is.element(attr.i, rownames(xc$data))
            index.out <- as.character(rownames(xc$data))
        }
        attrib.sub <- subset(attrib, exclude == TRUE)
        attrib.sub[] <- lapply(attrib.sub, function(x) x[drop = TRUE])

if(index == 0) {attr.s <- rownames(attrib.sub)} else {attr.s <- attrib.sub[, index]}
        attrib.out <- attrib.sub[order(as.character(attr.s)), ]

    }
    rownames(attrib.out) <- index.out
    attrib.out <- as.data.frame(attrib.out)

out <- list(
    data = attrib.out, 
    call = match.call(),
    which = which,
    check.setrel = check1,
    na = sum(is.na(attrib.out))
    )
    
    class(out) <- "xorder"
    return(out)
}

### print function for xorder
print.xorder <- function(x, ...) {
	cat("Object of class 'xorder'\n")
	cat("Call: ")
	print(x$call)
	cat("Attribute table for ")
		if(x$which == "samples") cat("samples\n") else cat("species\n")
	cat("Match of 'xcount' object and attribute table: ", x$check.setrel, "\n")
	cat("Attribute table contains ", ncol(x$data), "variables for", nrow(x$data))
	if(x$which == "samples") cat(" samples\n") else cat(" species\n")
	if(x$na == 0) 
		cat("NA values were not found in the table\n") else cat("Number of NA values in the table: ", x$na,"\n")
}
