## merges different columns from ABMI data sets
## x is a named list of vectors (or one column data frame)
abmi.merge <-
function(x, na.rm = FALSE, drop=FALSE)
{
   ## some preliminary chacks
   inherits(x, "list") || stop("'x' must be a list")
   n <- length(x)
   if (n < 2)
       stop("length of 'x' should be at least 2")
   ## store names
   nam <- names(x)
   names(x) <- NULL
   ## chack if all elements are named
   ncheck <- logical(n)
   for (i in 1:n) {
       ncheck[i] <- is.null(names(x[[i]]))
   }
   if (any(ncheck))
       stop("elements of 'x' must be named vectors")
   ## coerces elements into data frames
   x <- lapply(x, as.data.frame)
   ## bookeeping of factors
   f <- unlist(lapply(x, function(z) is.factor(z[,1])))
   ## making union of (row)names
   rnam <- rownames(x[[1]])
   for (i in 1:n) {
       rnam <- union(rnam, rownames(x[[i]]))
       if (isTRUE(f[i]))
           x[[i]][,1] <- as.character(x[[i]][,1])
   }
   ## build output, taking care of mode, placing NAs in empty cells
   out <- list()
   for (i in 1:n) {
       out[[i]] <- vector(mode = mode(x[[i]][,1]), length = length(rnam))
       out[[i]][match(rownames(x[[i]]), rnam)] <- x[[i]][,1]
       out[[i]][!(rnam %in% rownames(x[[i]]))] <- NA
       names(out)[i] <- nam[i]
   }
   ## put it together, and nemove NAs if na.rm = TRUE, drop unused levels
   out <- as.data.frame(out)
   rownames(out) <- rnam
   if (na.rm) {
       out <- out[apply(abmi.is.na(out), 1, sum) == 0,]
       out <- out[apply(is.na(out), 1, sum) == 0,]
   }
   if (drop)
       out[] <- lapply(out, function(a) a[drop = TRUE])
   return(out)
}
