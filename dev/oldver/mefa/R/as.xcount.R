`as.xcount` <-
function (table, species.columns=TRUE, segment="unspecified", digits=NULL, n=NULL){

if(class(table)=="xclist") {
    if (!is.null(n)) table <- table$data[[n]]$data
    if (is.null(n)) {
    xc <- table$data[[1]]$data
    for (i in 2:table$length) xc <- xc + table$data[[i]]$data
    table <- xc}}

if(class(table)=="mflist") table <- as.mefa(table, n)

if(class(table) == "mefa") {
    segment <- table$segment
    table <- table$data
    }

if(is.null(digits)) if(sum(table) != sum(trunc(table)))
    stop("count is not integer, use 'digits' argument")

if(!is.null(digits)) if(sum(table) != sum(trunc(table))) {
    table <- round(table, digits = digits)
    } else {digits <- NULL}

if(sum(is.na(table)) != 0) stop("NA values were detected")
if(species.columns == FALSE) table <- t(table)
table <- as.matrix(table[order(rownames(table)), order(colnames(table))])
out <- list(
	data = table,
	call = match.call(),
	segment = segment,
	digits = digits,
	nsamples = dim(table)[1],
	nspecies = dim(table)[2],
	totalcount = sum(table),
	presences = sum(table > 0),
	ninds = marmat(table, "samples", "abund"),
	srichn = marmat(table, "samples", "occur"),
	specabund = marmat(table, "species", "abund"),
	specoccur = marmat(table, "species", "occur"))
class(out) <- "xcount"
return(out)}

### print
print.xcount <- function(x, cutoff=25, ...) {

	cat("Object of class 'xcount'\n")
	cat("Call: ")
	print(x$call)
	cat("Data type: ")
		if(is.null(x$digits)) cat("count\n") else cat("measurement with ", x$digits, " digits\n")
	cat("Segment:", x$segment, "\n")
	cat("Number of samples: ", x$nsamples, "\n")
	cat("Number of species: ", x$nspecies, "\n")
	if(is.null(x$digits)) cat("Total count : ") else cat("Sum of all measurement: ")
		cat(x$totalcount, "\n")
	cat("Matrix fill: ", round(x$presences/(x$nsamples*x$nspecies),digits=3),
		"with (", x$presences, " presences)\n")
	if(any(x$ninds == 0)){
		if(length(x$ninds == 0) > cutoff){
		cat("Samples with zero total count [1:",cutoff,"]: \n")
		print(names(x$ninds)[which(x$ninds == 0)])
                cat("...\n")
                } else {
		cat("Samples with zero total count: \n")
		print(names(x$ninds)[which(x$ninds == 0)])}
                }
	if(any(x$specabund == 0)){
		if(length(x$specabund == 0) > cutoff){
		cat("Species with zero total count [1:",cutoff,"]: \n")
		print(names(x$ninds)[which(x$ninds == 0)])
                cat("...\n")
                } else {
		cat("Species with zero total count: \n")
		print(names(x$specabund)[which(x$specabund == 0)])}
                }
}

### plot
plot.xcount <- function(x, type="hist", rug=FALSE, logscale=FALSE, ...) {

main.A <- "Species richness"
main.B <- "Species occurences"
main.C <- "Number of individuals"
main.D <- "Species abundances"
sub.A <- "(within samples)"
sub.B <- "(within species)"
sub.C <- "(within samples)"
sub.D <- "(within species)"

plot.A <- plot.B <- plot.C <- plot.D <- TRUE
layout <- c(2,2)
if(x$totalcount == x$presences){
	plot.C <- plot.D <- FALSE
	layout <- c(1,2)
	}

if(logscale){
        srichn <- log10(x$srichn + 1)
        specoccur <- log10(x$specoccur + 1)
        ninds <- log10(x$ninds + 1)
        specabund <- log10(x$specabund + 1)
        } else {
        srichn <- x$srichn
        specoccur <- x$specoccur
        ninds <- x$ninds
        specabund <- x$specabund
        }

if(type == "hist"){
	par(mfrow=layout, pty="s")
		if(plot.A) {
			hist(srichn, 
			main=main.A, sub=sub.A, xlab="Species richness", ...)
			if(rug) rug(x$srichn)
			}
		if(plot.B) {
			hist(specoccur, 
			main=main.B, sub=sub.B, xlab="Occurence", ...)
			if(rug) rug(x$specoccur)
			}
		if(plot.C) {
			hist(ninds, 
			main=main.C, sub=sub.C, xlab="Number of individuals", ...)
			if(rug) rug(x$ninds)
			}
		if(plot.D) {
			hist(specabund, 
			main=main.D, sub=sub.D, xlab="Abundance", ...)
			if(rug) rug(x$specabund)
			}
	par(mfrow=c(1,1))
	}

if(type == "rank"){
	xlab="Rank"
	par(mfrow=layout, pty="s")
		if(plot.A) {
			plot(sort(srichn, decreasing = TRUE), type="b", 
			main=main.A, sub=sub.A, xlab=xlab, ylab="Richness", ...)
			}
		if(plot.B) {
			plot(sort(specoccur, decreasing = TRUE), type="b", 
			main=main.B, sub=sub.B, xlab=xlab, ylab="Occurence", ...)
			}
		if(plot.C) {
			plot(sort(ninds, decreasing = TRUE), type="b", 
			main=main.C, sub=sub.C, xlab=xlab, ylab="Number of individuals", ...)
			}
		if(plot.D) {
			plot(sort(specabund, decreasing = TRUE), type="b", 
			main=main.D, sub=sub.D, xlab=xlab, ylab="Abundance", ...)
			}
	par(mfrow=c(1,1))
	}

if(type == "biplot"){
	if(x$totalcount == x$presences) stop("no abundance data for biplot")
	par(mfrow=c(1,2), pty="s")
		plot(ninds, srichn,
			main="Within samples", xlab="Number of individuals", ylab="Species richness", ...)
		plot(specabund, specoccur,
			main="Within species", xlab="Abundance", ylab="Occurence", ...)
	par(mfrow=c(1,1))
	}
}
