## creates numeric  day values from character ones
juliandaycounter <-
function(x, format="%d-%b-%y", yr.start=8, yr.end=9, na.rm=FALSE)
{
    if (NCOL(x) != 1)
        stop("'x' must be vector")
    x <- as.character(x)
    naid <- abmi.is.na(x)
    fulldate <- x[!naid]
    if (any(nchar(fulldate) != 9))
        stop("not adequate date format")
    year <- paste("20", substr(fulldate, yr.start, yr.end), sep="")
    jan1 <- as.Date(unlist(lapply(year, function(x) paste(x,"-01-01", sep=""))))
    fulldate <- as.Date(fulldate, format)
    out <- as.numeric(fulldate) - as.numeric(jan1)
    if (na.rm) {
        return(out)
        } else {
            y <- numeric(length(x))
            y[!naid] <- out
            y[naid] <- NA
            return(y)
        }
}
