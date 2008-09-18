`amf` <-
function(x, which="samples", attach=FALSE){
if (class(x) != "mefa") stop("Object '", x, "' is not of 'mefa' class.")
ninds <- x$ninds
srichn <- x$srichn
specabund <- x$specabund 
specoccur <- x$specoccur
if (which=="samples") {
    if (is.null(x$sample.attr)) stop("Sample attribute table is NULL")
    tab <- data.frame(ninds, srichn, x$sample.attr)}
if (which=="species") {
    if (is.null(x$species.attr)) stop("Species attribute table is NULL")
    tab <- data.frame(specabund, specoccur, x$species.attr)}
if(attach) {
    attach(tab)
    return(tab)
    } else return(tab)}
