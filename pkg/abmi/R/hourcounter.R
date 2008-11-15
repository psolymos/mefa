## creates numeric hour values from character ones
hourcounter <-
function(x, with.min=TRUE, with.sec=FALSE, na.rm=FALSE)
{
    require(chron)
    hour <- x
    if (!with.min && with.sec)
        stop("'with.sec' can not be 'TRUE' if 'with.min = FALSE'")
    if (NCOL(hour) != 1)
        stop("'x' must be vector")
    addchar <- if (with.min && with.sec)
        ":00:00" else ":00"
    hour <- as.character(hour)
    naid <- abmi.is.na(hour)
    hour2 <- hour[!naid]

    if (!with.sec || !with.min)
        hour2 <- unlist(lapply(hour2, function(x) paste(x, addchar, sep="")))
    hour2[nchar(hour2) < 8] <- paste("0", hour2[nchar(hour2) < 8], sep="")
    if (any(nchar(hour2) != 8))
        stop("< 10 minutes part format is wrong (i.e. '1' not '01')")
    hour2 <- as.numeric(times(hour2)) * 24
    if (na.rm) {
        hour <- hour2
        } else {
            hour[!naid] <- hour2
            hour[naid] <- NA
        }
    return(as.numeric(hour))
}
