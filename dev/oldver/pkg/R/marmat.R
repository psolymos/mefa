`marmat` <-
function(x, which = c("samples", "species"), method=c("abund", "occur"), group=NULL){

if(which == "samples") margin <- 1
if(which == "species") margin <- 2

if(is.null(group)) {
	if(which == "samples") group.by <- c(1:nrow(x))
	if(which == "species") group.by <- c(1:ncol(x))
	} else {group.by <- group}

if(which == "samples") tab <- aggregate(x, list(group.by), sum)[,-1]
if(which == "species") tab <- t(aggregate(t(x), list(group.by), sum)[,-1])

if(method == "abund") out <- apply(tab, MARGIN = margin, sum)
if(method == "occur") out <- apply(tab > 0, MARGIN = margin, sum)

if(is.null(group)) {
	if(which == "samples") names(out) <- rownames(x)
	if(which == "species") names(out) <- colnames(x)
	} else {names(out) <- levels(as.factor(group))}

return(out)
}

