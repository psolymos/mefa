`strify` <-
function(xc, strata, which=c("samples", "species")){

if(is.element(class(xc), c("xcount", "mefa")) == FALSE) 
	stop("Object '",xc,"' is not of 'xcount' or 'mefa' class.")
if(is.element(which, c("samples", "species")) == FALSE)
	stop("Undefined 'which' tag.")

if(class(xc) == "xcount") {
	ifelse(which == "samples", table <- xc$data, table <- t(xc$data))
	str <- as.factor(strata)}

if(class(xc) == "mefa") {
	if(which == "samples") {
		if(is.null(xc$sample.attr)) stop("Stratifying NULL attribute table of 'mefa' object has no sense.")
		table <- xc$data
		str <- as.factor(xc$sample.attr[,strata])
		} else {
		if(is.null(xc$species.attr)) stop("Stratifying NULL attribute table of 'mefa' object has no sense.")
		table <- t(xc$data)
		str <- as.factor(xc$species.attr[,strata])}
	}

if(nrow(table) == 1) {
	if(which =="samples"){
	stop("One sample can not be stratified.")
	} else {
	stop("One species can not be stratified.")
	}
}
if(nlevels(str) == 1) stop("More than only one strata is needed.")

table.out <- array(data=NA, dim=c(nlevels(str), dim(table)[2]))
for(i in 1:dim(table.out)[2]){table.out[,i] <- aggregate(table[,i], list(str), sum)[[2]]}
colnames(table.out) <- colnames(table)
rownames(table.out) <- levels(str)
if(which == "samples") {data <- as.matrix(table.out)} else {data <- as.matrix(t(table.out))}
data <- data[order(as.character(rownames(data))), order(as.character(colnames(data)))]

newxc <- as.xcount(data, segment=xc$segment)
out <- newxc

if(class(xc) == "mefa"){
if(which == "samples" & !is.null(xc$species.attr)){
	out <- mefa(newxc, NULL, xorder(newxc, "species", xc$species.attr))}
if(which == "species" & !is.null(xc$sample.attr)){
	out <- mefa(newxc, xorder(newxc, "samples", xc$sample.attr), NULL)}
	}

out$call <- match.call()
return(out)}

