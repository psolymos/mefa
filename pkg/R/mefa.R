`mefa` <-
function(xc, xorder.samples, xorder.species){

if(class(xc) != "xcount") stop("Object '",xc,"' is not of 'xcount' class.")

if(is.null(xorder.samples) & is.null(xorder.species)) stop("at least one 'xorder' argument must be specified")

attributes <- "both"

cxc <- colnames(xc$data)
rxc <- rownames(xc$data)

if(!is.null(xorder.species)){
	if(class(xorder.species) != "xorder") stop("Object '",xorder.species,"' is not of 'xorder' class.")
	if(xorder.species$which != "species") stop("Species/sample mismatch.")
	rspec <- rownames(xorder.species$data)
	if(length(rspec) != length(cxc)) stop("Species list lengths differ.")
	if(sum(rspec != cxc) != 0) stop("Species list mismatch.")
	} else {attributes <- "samples.only"}

if(!is.null(xorder.samples)){
	if(class(xorder.samples) != "xorder") stop("Object '",xorder.samples,"' is not of 'xorder' class.")
	if(xorder.samples$which != "samples") stop("Species/sample mismatch.")
	rsamp <- rownames(xorder.samples$data)
	if(length(rsamp) != length(rxc)) stop("Sample list lengths differ.")
	if(sum(rsamp != rxc) != 0) stop("Sample list mismatch.")
	} else {attributes <- "species.only"}


am <- matrix(NA, 2, 3)
if(!is.null(xorder.samples)) {
    am[1,1] <- xorder.samples$check.setrel
    am[1,2] <- ncol(xorder.samples$data)
    am[1,3] <- xorder.samples$na
    }
if(!is.null(xorder.species)) {
    am[2,1] <- xorder.species$check.setrel
    am[2,2] <- ncol(xorder.species$data)
    am[2,3] <- xorder.species$na
    }
rownames(am) <- c("sample.attr", "species.attr")
colnames(am) <- c("check.setrel", "variables", "na")

new <- list(
	sample.attr = if(!is.null(xorder.samples)) xorder.samples$data else NULL,
	species.attr = if(!is.null(xorder.species)) xorder.species$data else NULL,
	attributes = attributes,
	attrib.matrix = am
	)

out <- c(xc[1], new[c(1,2)], xc[c(2:12)], new[c(3,4)])
out$call <- match.call()
class(out) <- "mefa"
return(out)}

### print
print.mefa <- function(x, ...) {
	cat("Object of class 'mefa'\n")
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
	if(x$attributes == "both")
		{cat("Both attribute tables are attached: \n")
		print(x$attrib.matrix)}
	if(x$attributes == "samples.only")
		{cat("Only sample attribute table is attached: \n")
		print(x$attrib.matrix[1,])}
	if(x$attributes == "species.only")
		{cat("Only species attribute table is attached: \n")
		print(x$attrib.matrix[2,])}
	if(x$attributes != "species.only"){
		cat("Variables in the sample attribute table: \n")
		print(colnames(x$sample.attr))}
	if(x$attributes != "samples.only"){
		cat("Variables in the species attribute table: \n")
		print(colnames(x$species.attr))}
}

### plot
#x<- vmf; sample.var="site.descr"; species.var="shell.dimension"
#x<- vmf; sample.var=3; species.var=5

plot.mefa <- function(x, sample.var=NULL, species.var=NULL, ...){
if(is.null(sample.var) & is.null(species.var)) {
	plot(as.xcount(x), ...)} else {

if(is.character(sample.var)) sample.var <- which(colnames(x$sample.attr) == sample.var)
if(is.character(species.var)) species.var <- which(colnames(x$species.attr) == species.var)

main.A <- "Species richness"
main.B <- "Species occurences"
main.C <- "Number of individuals"
main.D <- "Species abundances"
xlab.A <- xlab.C <- paste(colnames(x$sample.attr)[sample.var])
xlab.B <- xlab.D <- paste(colnames(x$species.attr)[species.var])

if(!is.null(sample.var)){
sampdf <- data.frame(
	srichn <- x$srichn,
	ninds <- x$ninds,
	sample.variable <- x$sample.attr[sample.var])
	}

if(!is.null(species.var)){
specdf <- data.frame(
	specoccur <- x$specoccur,
	specabund <- x$specabund,
	species.variable <- x$species.attr[species.var])
	}

plot.A <- plot.B <- plot.C <- plot.D <- TRUE
layout <- c(2,2)

if(is.null(sample.var)) {
	plot.A <- plot.C <- FALSE
	layout <- c(1,2)}
if(is.null(species.var)) {
	plot.B <- plot.D <- FALSE
	layout <- c(1,2)}

if(x$totalcount == x$presences){
	plot.C <- plot.D <- FALSE
	layout <- c(1,2)
	if(is.null(sample.var)) layout <- c(1,1)
	if(is.null(species.var)) layout <- c(1,1)
	}

par(mfrow=layout, pty="s")
	if(plot.A) {
		plot(sampdf[,1]~sampdf[,3],
		main=main.A, xlab=xlab.A, ylab="Species richness", ...)
		}
	if(plot.B) {
		plot(specdf[,1]~specdf[,3],
		main=main.B, xlab=xlab.B, ylab="Occurence", ...)
		}
	if(plot.C) {
		plot(sampdf[,2]~sampdf[,3],
		main=main.C, xlab=xlab.C, ylab="Number of individuals", ...)
		}
	if(plot.D) {
		plot(specdf[,2]~specdf[,3],
		main=main.D, xlab=xlab.D, ylab="Abundance", ...)
		}
par(mfrow=c(1,1))

}}

