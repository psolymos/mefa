`xcount` <-
function (ssc, segment = 0, segment.name=NULL) 
{
    if (class(ssc) != "sscount") 
        stop("Object '", ssc, "' is not of 'sscount' class.")
	if(length(segment) > length(ssc$segment.levels))
		stop("segment length greater than levels of segment")

if(segment != 0 & segment != "all"){
	if(length(segment) == length(ssc$segment.levels)) {
		if(is.numeric(segment)) segment.eval <- ssc$segment.levels[segment]
		if(is.character(segment)) segment.eval <- segment
		if(setequal(segment.eval, ssc$segment.levels)) {
		segment <- 0} else {stop("segment level mismatch")}
		}
}

#	if(sum(ssc$data$count*10^ssc$digits) >= 10000) {
#		cat("10^digits * total count =", 10^ssc$digits, "*", sum(ssc$data$count), "=", 
#			sum(ssc$data$count*10^ssc$digits))
#		cat("\ninflating data, this may take several seconds ...\n")}
	if(!is.null(ssc$digits)) ssc$data$count <- ssc$data$count*10^trunc(ssc$digits)

    infl <- inflate(ssc$data[, 1:3], ssc$data[, 4])

	if(length(segment) == 1){
	if(segment == 0 | segment == "all") {
        crosstable <- as.array(table(infl[, 1], infl[, 2]))
        segment.index <- "all"
		} else {
		segnew <- infl[,3]
		crosstable <- as.array(table(infl[, 1], infl[, 2], segnew))[, , segment]
		if(is.character(segment)) segment <- which(ssc$segment.levels == segment)
		segment.index <- ssc$segment.levels[segment]
		}
	} else {
		if(is.null(segment.name)) segment.name <- "mixed"
		if(is.numeric(segment)) segment <- ssc$segment.levels[segment]
		segnew <- as.factor(infl[,3])
		levels(segnew)[which(is.element(levels(segnew), segment))] <- segment.name
		seg.id <- which((levels(segnew) == segment.name) == TRUE)
		crosstable <- as.array(table(infl[, 1], infl[, 2], segnew))[, , seg.id]
		segment.index <- segment.name
	}

	if(!is.null(ssc$digits)) crosstable <- crosstable/(10^ssc$digits)

	if(is.null(ssc$zc) == FALSE) crosstable <- crosstable[,-which(colnames(crosstable) == ssc$zc)]

    out <- as.xcount(crosstable, species.columns=TRUE, segment=segment.index, digits=ssc$digits)
    out$call <- match.call()
    return(out)
}

