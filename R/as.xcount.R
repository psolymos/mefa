`as.xcount` <-
function (table, species.columns=TRUE, segment="unspecified", digits=NULL){

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
print.xcount <- function(x, ...) {

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
		cat("Samples with zero total count: \n")
		print(names(x$ninds)[which(x$ninds == 0)])}
	if(any(x$specabund == 0)){
		cat("Species with zero total count: \n")
		print(names(x$specabund)[which(x$specabund == 0)])}
}

### plot
plot.xcount <- function(x, type="hist", rug=FALSE, ...) {

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

if(type == "hist"){
	par(mfrow=layout, pty="s")
		if(plot.A) {
			hist(x$srichn, 
			main=main.A, sub=sub.A, xlab="Species richness", ...)
			if(rug) rug(x$srichn)
			}
		if(plot.B) {
			hist(x$specoccur, 
			main=main.B, sub=sub.B, xlab="Occurence", ...)
			if(rug) rug(x$specoccur)
			}
		if(plot.C) {
			hist(x$ninds, 
			main=main.C, sub=sub.C, xlab="Number of individuals", ...)
			if(rug) rug(x$ninds)
			}
		if(plot.D) {
			hist(x$specabund, 
			main=main.D, sub=sub.D, xlab="Abundance", ...)
			if(rug) rug(x$specabund)
			}
	par(mfrow=c(1,1))
	}

if(type == "rank"){
	xlab="Rank"
	par(mfrow=layout, pty="s")
		if(plot.A) {
			plot(sort(x$srichn, decreasing = TRUE), type="b", 
			main=main.A, sub=sub.A, xlab=xlab, ylab="Richness", ...)
			}
		if(plot.B) {
			plot(sort(x$specoccur, decreasing = TRUE), type="b", 
			main=main.B, sub=sub.B, xlab=xlab, ylab="Occurence", ...)
			}
		if(plot.C) {
			plot(sort(x$ninds, decreasing = TRUE), type="b", 
			main=main.C, sub=sub.C, xlab=xlab, ylab="Number of individuals", ...)
			}
		if(plot.D) {
			plot(sort(x$specabund, decreasing = TRUE), type="b", 
			main=main.D, sub=sub.D, xlab=xlab, ylab="Abundance", ...)
			}
	par(mfrow=c(1,1))
	}

if(type == "biplot"){
	if(x$totalcount == x$presences) stop("no abundance data for biplot")
	par(mfrow=c(1,2), pty="s")
		plot(x$ninds, x$srichn,
			main="Within samples", xlab="Number of individuals", ylab="Species richness", ...)
		plot(x$specabund, x$specoccur,
			main="Within species", xlab="Abundance", ylab="Occurence", ...)
	par(mfrow=c(1,1))
	}
}
