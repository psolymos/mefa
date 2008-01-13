`add.attrib` <-
function(mf, which=c("samples", "species"), attrib, index=0){

if(class(mf) != "mefa") stop("Object '",mf,"' is not of 'mefa' class.")

xord <- xorder(as.xcount(mf$data), which, attrib, index)

if(index != 0) xord$data[[index]] <- NULL

if(which == "species") {
	xsamp.out <- mf$sample.attr
	xspec.out <- data.frame(mf$species.attr, xord$data)
	nsample.attr <- mf$nsample.attr
	nspecies.attr <- dim(xspec.out)[2]}

if(which == "samples") {
	xsamp.out <- data.frame(mf$sample.attr, xord$data)
	xspec.out <- mf$species.attr
	nsample.attr <- dim(xsamp.out)[2]
	nspecies.attr <- mf$nspecies.attr}

xco <- as.xcount(mf$data)
xo1 <- xorder(xco, "samples", xsamp.out)
xo2 <- xorder(xco, "species", xspec.out)
out <- mefa(xco, xo1, xo2)
return(out)}

