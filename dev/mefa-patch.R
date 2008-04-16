###
as.mefa <- function(mfl, n=1){
if(!inherits(mfl,"mflist")) stop("Object is not of class 'mfcount'.")
xc <- mfl$data[[n]]
if(is.null(mfl$sample.attr)) xo1 <- NULL else xo1 <- xorder(xc, "samples", mfl$sample.attr)
if(is.null(mfl$species.attr)) xo2 <- NULL else xo2 <- xorder(xc, "species", mfl$species.attr)
return(mefa(xc, xo1, xo2))}

###
allosym <-
function(x, y)
{
x <- x > 0
y <- y > 0
a <- as.numeric(x & y)
b <- as.numeric(x & !y)
c <- as.numeric(!x & y)
d <- as.numeric(!x & !y)
return(cbind(a,b,c,d))}
# example (vs 
#x <- c(1,1,1,0,0,0)
#y <- c(1,1,0,1,1,0)
#allosym(x, y)

###
read.nucosa <- 
function(filename, digits=NULL, segment = "unspecified", dec = ".")
{
nuc.title <- as.character(read.table(file = filename, skip = 0, nrows = 1, sep="\n")[1,1])
nuc.dim <- scan(file = filename, skip = 1, nlines = 1, sep=" ")
nuc.data <- matrix(
    scan(file = filename, skip = 2, n=(nuc.dim[1]*nuc.dim[2]), sep=" ", dec=dec),
    nuc.dim[2], nuc.dim[1])
nuc.names <- read.table(file = filename, skip = (nuc.dim[1]+2), sep="\n")
colnames(nuc.data) <- t(nuc.names)
rownames(nuc.data) <- c(1:nuc.dim[2])
    cat("Input file '",filename,"' entitled:\n")
    print(nuc.title)
    cat("was converted to 'xcount' format.\n")
return(as.xcount(nuc.data, digits=digits, segment=segment))}

### sample data generation ... very experimental, DO NOT include!
sample.sscount <-
function(size, lambda1=3, lambda2=(4*lambda1), probs=seq(0, 1, 0.25))
{

samples <- rep(1:(size/(lambda1-1)),times=rpois((size/(lambda1-1)),lambda1))
species <- rep(1:(size/(lambda2-1)),times=rpois((size/(lambda2-1)),lambda2))
sampledat <- data.frame(
    sample(samples,size),
    sample(species,size),
    qdef(c(1:size),probs = probs),
    rep(1,size) )

return(sscount(sampledat))
}

### mflist
mflist <- 
function(xclist, xo1, xo2){
mf <- mefa(xclist$data[[1]], xo1, xo2)

out <- list(
	data=xclist$data,
	sample.attr=mf$sample.attr,
	species.attr=mf$species.attr,
	call=match.call(),
	segment=xclist$segment,
	nested=xclist$nested,
	length=xclist$length,
	probs=xclist$probs,
	digits=mf$digits,
	nsamples=xclist$nsamples,
	nspecies=xclist$nspecies,
	attributes=mf$attributes,
	attrib.matrix=mf$attrib.matrix)

class(out) <- "mflist"
return(out)}

###
print.mflist <-
function (x, ...) {
    cat("Object of class 'mflist'\n")
    cat("Call: ")
    print(x$call)
    if(x$nested) cat("Nested partitions") else cat("Separate partitions")
    cat("\nLength: ", x$length, "\n")
    if(is.null(x$probs)) cat("Probs: NULL\n")
        else {cat("Probs:\n")
            print(x$probs)}
    cat("Data type: ")
    if (is.null(x$digits)) 
        cat("count\n")
    else cat("measurement with ", x$data[[1]]$digits, " digits\n")
    cat("Number of samples: ", x$nsamples, "\n")
    cat("Number of species: ", x$nspecies, "\n")
    cat("Segments:\n")
    print(x$segment)
    if (x$attributes == "both") {
        cat("Both attribute tables are attached: \n")
        print(x$attrib.matrix)
    }
    if (x$attributes == "samples.only") {
        cat("Only sample attribute table is attached: \n")
        print(x$attrib.matrix[1, ])
    }
    if (x$attributes == "species.only") {
        cat("Only species attribute table is attached: \n")
        print(x$attrib.matrix[2, ])
    }
    if (x$attributes != "species.only") {
        cat("Variables in the sample attribute table: \n")
        print(colnames(x$sample.attr))
    }
    if (x$attributes != "samples.only") {
        cat("Variables in the species attribute table: \n")
        print(colnames(x$species.attr))
    }
}


###
plot.mflist <- function(x, n=1, ...){
xc <- x$data[[n]]
if(is.null(x[[2]])) xo1 <- NULL else xo1 <- xorder(xc,"samples",x$sample.attr)
if(is.null(x[[3]])) xo2 <- NULL else xo2 <- xorder(xc,"species",x$species.attr)
mf <- mefa(xc,xo1,xo2)
plot(mf, ...)
}

###fun xclist
xclist <- 
function(ssc, nested=TRUE, probs = seq(0, 1, 0.25)){
    if (class(ssc) != "sscount") 
        stop("Object '", ssc, "' is not of 'sscount' class.")

if(sum(ssc$data[ ,4]) != length(ssc$data[ ,4])) {
    ssc$data$segment[is.na(ssc$data$segment)] <- levels(ssc$data$segment)[1]
    ssc <- sscount(data.frame(inflate(ssc$data[, 1:3], ssc$data[, 4]), rep(1,sum(ssc$data[, 4]))),
        zc=ssc$zc, digits=ssc$digits)
    }

if(!is.null(probs)) {ssc2 <- sscount(
    data.frame(ssc$data$sample,ssc$data$species,
    qdef(as.numeric(ssc$data$segment),probs),ssc$data$count))
    } else {ssc2 <- ssc}
nseg <- length(ssc2$segment.levels)
seglist <- ssc2$segment.levels
outdat <- list()
for(i in 1:nseg) {
    #cat("Processing segment",i,"of",nseg,"\n")
    if(nested) segm <- c(1:i) else segm <- i
    outdat[[i]] <- xcount(ssc2,segm)
    outdat[[i]]$segment <- seglist[i]}
if(nested) probsout <- probs else probsout <- NULL 
out <- list(data=outdat,call=match.call(),nested=nested,
    length=length(outdat),probs=probsout,segment=seglist,
    digits=outdat[[1]]$digits,nsamples=outdat[[1]]$nsamples,nspecies=outdat[[1]]$nspecies)
class(out) <- "xclist"
return(out)}


## addition due to xclist efficiency (inflate loop)
xcount <-
function (ssc, segment = 0, segment.name = NULL) 
{
    if (class(ssc) != "sscount") 
        stop("Object '", ssc, "' is not of 'sscount' class.")
    if (length(segment) > length(ssc$segment.levels)) 
        stop("segment length greater than levels of segment")
    if (length(segment) == length(ssc$segment.levels)) {
        if (length(segment) == 1 & length(ssc$segment.levels) == 
            1) {
            if (segment == 0 | segment == "all" | segment == 
                1 | segment == ssc$segment.levels[1]) {
                segment <- 0
            }
            else {
                stop("segment level mismatch")
            }
        }
        else {
            if (is.numeric(segment)) 
                segment.eval <- ssc$segment.levels[segment]
            if (is.character(segment)) 
                segment.eval <- segment
            if (setequal(segment.eval, ssc$segment.levels)) {
                segment <- 0
            }
            else {
                stop("segment level mismatch")
            }
        }
    }
    if (!is.null(ssc$digits)) 
        ssc$data$count <- ssc$data$count * 10^trunc(ssc$digits)
##
if(sum(ssc$data[ ,4]) == length(ssc$data[ ,4])) infl <- ssc$data[, 1:3]
else infl <- inflate(ssc$data[, 1:3], ssc$data[, 4])
##
    if (length(segment) == 1) {
        if (segment == 0 | segment == "all") {
            crosstable <- as.array(table(infl[, 1], infl[, 2]))
            segment.index <- "all"
        }
        else {
            segnew <- infl[, 3]
            crosstable <- as.array(table(infl[, 1], infl[, 2], 
                segnew))[, , segment]
            if (is.character(segment)) 
                segment <- which(ssc$segment.levels == segment)
            segment.index <- ssc$segment.levels[segment]
        }
    }
    else {
        if (is.null(segment.name)) 
            segment.name <- "mixed"
        if (is.numeric(segment)) 
            segment <- ssc$segment.levels[segment]
        segnew <- as.factor(infl[, 3])
        levels(segnew)[which(is.element(levels(segnew), segment))] <- segment.name
        seg.id <- which((levels(segnew) == segment.name) == TRUE)
        crosstable <- as.array(table(infl[, 1], infl[, 2], segnew))[, 
            , seg.id]
        segment.index <- segment.name
    }
    if (!is.null(ssc$digits)) 
        crosstable <- crosstable/(10^ssc$digits)
    if (is.null(ssc$zc) == FALSE) 
        crosstable <- crosstable[, -which(colnames(crosstable) == 
            ssc$zc)]
    out <- as.xcount(crosstable, species.columns = TRUE, segment = segment.index, 
        digits = ssc$digits)
    out$call <- match.call()
    return(out)
}

#########
print.xclist <- 
function (x, ...) 
{
    cat("Object of class 'xclist'\n")
    cat("Call: ")
    print(x$call)
    if(x$nested) cat("Nested partitions") else cat("Separate partitions")
    cat("\nLength: ", x$length, "\n")
    if(is.null(x$probs)) cat("Probs: NULL\n")
        else {cat("Probs:\n")
            print(x$probs)}
    cat("Data type: ")
    if (is.null(x$digits)) 
        cat("count\n")
    else cat("measurement with ", x$data[[1]]$digits, " digits\n")
    cat("Number of samples: ", x$nsamples, "\n")
    cat("Number of species: ", x$nspecies, "\n")
    cat("Segments:\n")
    print(x$segment)
}

###
plot.xclist <- function(x, n=1, ...){
plot(x$data[[n]], ...)
}

### ANIMEFA
animefa <- 
function(xclist, method=c("biplot","hist"), type="both",
    reference=xclist$length, logscale=FALSE, abundance=FALSE,
    control=ani.control(interval=1), bg=NULL, pty="m", axes=FALSE, freq=TRUE, ...)
{

if (class(xclist) != "xclist" & class(xclist) != "mflist") 
    stop("Class of object '", xc, "' is not appropriate.")

require(animation)
control <- checkargs(control, ...)

### set scales
xmax1 <- xmax2 <- ymax1 <- ymax2 <- rep(0,xclist$length)
for(i in 1:mfl$length) {
    xmax1[i] <- max(xclist$data[[i]]$srichn)
    ymax1[i] <- max(xclist$data[[i]]$ninds)
    xmax2[i] <- max(xclist$data[[i]]$specoccur)
    ymax2[i] <- max(xclist$data[[i]]$specabund)
    }
if(logscale) xmax1 <- log10(max(xmax1)+1) else xmax1 <- max(xmax1)
if(logscale) ymax1 <- log10(max(ymax1)+1) else ymax1 <- max(ymax1)
if(logscale) xmax2 <- log10(max(xmax2)+1) else xmax2 <- max(xmax2)
if(logscale) ymax2 <- log10(max(ymax2)+1) else ymax2 <- max(ymax2)

if(logscale) logchar <- "Log10" else logchar <- ""
if(method=="biplot"){
    xlabtemp1 <- paste(logchar, "Species per sample")
    xlabtemp2 <- paste(logchar, "Samples per species")}
if(method=="hist"){
    if(abundance){
        xlabtemp1 <- paste(logchar, "Relative number of records")
        xlabtemp2 <- paste(logchar, "Relative species abundances")}
    if(!abundance){
        xlabtemp1 <- paste(logchar, "Relative number of species")
        xlabtemp2 <- paste(logchar, "Relative species incidences")}
    }
for(i in 1:xclist$length){

if(method=="biplot"){
if(is.null(bg)) coldef <- heat.colors(xclist$length)[(xclist$length+1-i)] else coldef <- bg
    if(logscale) xtemp1 <- log10(xclist$data[[i]]$srichn) else xtemp1 <- xclist$data[[i]]$srichn
    if(logscale) ytemp1 <- log10(xclist$data[[i]]$ninds) else ytemp1 <- xclist$data[[i]]$ninds

    if(logscale) xtemp2 <- log10(xclist$data[[i]]$specoccur) else xtemp2 <- xclist$data[[i]]$specoccur
    if(logscale) ytemp2 <- log10(xclist$data[[i]]$specabund) else ytemp2 <- xclist$data[[i]]$specabund

	if(type=="both") par(mfrow=c(1,2),pty=pty)
	if(type=="both" | type=="samples")
	  plot(xtemp1,ytemp1,bg=coldef,pch=21,cex=1.5,main="Samples",ylab=paste(logchar, "Records per sample"),
		xlab=xlabtemp1,xlim=c(0,xmax1),ylim=c(0,ymax1))
	if(type=="both" | type=="species") 
	  plot(xtemp2,ytemp2,bg=coldef,pch=21,cex=1.5,main="Species",ylab=paste(logchar, "Records per species"),
		xlab=xlabtemp2,xlim=c(0,xmax2),ylim=c(0,ymax2))
	if(type=="both") par(mfrow=c(1,1))
} # end biplot method

if(method=="hist"){
if(is.null(bg)) coldef <- heat.colors(10)[10:1] else coldef <- bg
	if(type=="both") par(mfrow=c(1,2),pty=pty)

if(!abundance){
	samples <- xclist$data[[i]]$srichn/xclist$data[[reference]]$srichn
	species <- xclist$data[[i]]$specoccur/xclist$data[[reference]]$specoccur}
if(abundance){
	samples <- log10(xclist$data[[i]]$ninds)/log10(xclist$data[[reference]]$ninds)
	species <- log10(xclist$data[[i]]$specabund)/log10(xclist$data[[reference]]$specabund)}

if(i != reference){
	hist(samples, freq=freq, xlab=xlabtemp1, col=coldef, axes=axes)
	hist(species, freq=freq, xlab=xlabtemp2, col=coldef, axes=axes)
	}

	if(type=="both") par(mfrow=c(1,1))
} # end hist method

if (control$saveANI) 
    savePNG(n = i, width = control$width, height = control$height)
else Sys.sleep(control$interval)
}
invisible(NULL)
}

### fix

print.xcount <-						########### FIX print.mefa !!!!!!!!!
function (x, cutoff = 25, ...) 
{
    cat("Object of class 'xcount'\n")
    cat("Call: ")
    print(x$call)
    cat("Data type: ")
    if (is.null(x$digits)) 
        cat("count\n")
    else cat("measurement with ", x$digits, " digits\n")
    cat("Segment:", x$segment, "\n")
    cat("Number of samples: ", x$nsamples, "\n")
    cat("Number of species: ", x$nspecies, "\n")
    if (is.null(x$digits)) 
        cat("Total count : ")
    else cat("Sum of all measurement: ")
    cat(x$totalcount, "\n")
    cat("Matrix fill: ", round(x$presences/(x$nsamples * x$nspecies), 
        digits = 3), "with (", x$presences, " presences)\n")
    if (any(x$ninds == 0)) {
        if (sum(x$ninds == 0) > cutoff) {							# FIX
            cat("Samples with zero total count [1:", cutoff, 
                "]: \n")
            print(names(x$ninds)[which(x$ninds == 0)][1:cutoff])	# FIX
            cat("...\n")
        }
        else {
            cat("Samples with zero total count: \n")
            print(names(x$ninds)[which(x$ninds == 0)])
        }
    }
    if (any(x$specabund == 0)) {
        if (sum(x$specabund == 0) > cutoff) {						# FIX
            cat("Species with zero total count [1:", cutoff, 
                "]: \n")
            print(names(x$specabund)[which(x$specabund == 0)][1:cutoff])		# FIX
            cat("...\n")
        }
        else {
            cat("Species with zero total count: \n")
            print(names(x$specabund)[which(x$specabund == 0)])
        }
    }
}

print.mefa <- 
function (x, cutoff = 25, ...) 
{
    cat("Object of class 'mefa'\n")
    cat("Call: ")
    print(x$call)
    cat("Data type: ")
    if (is.null(x$digits)) 
        cat("count\n")
    else cat("measurement with ", x$digits, " digits\n")
    cat("Segment:", x$segment, "\n")
    cat("Number of samples: ", x$nsamples, "\n")
    cat("Number of species: ", x$nspecies, "\n")
    if (is.null(x$digits)) 
        cat("Total count : ")
    else cat("Sum of all measurement: ")
    cat(x$totalcount, "\n")
    cat("Matrix fill: ", round(x$presences/(x$nsamples * x$nspecies), 
        digits = 3), "with (", x$presences, " presences)\n")
    if (any(x$ninds == 0)) {
        if (sum(x$ninds == 0) > cutoff) {							# FIX
            cat("Samples with zero total count [1:", cutoff, 
                "]: \n")
            print(names(x$ninds)[which(x$ninds == 0)][1:cutoff])	# FIX
            cat("...\n")
        }
        else {
            cat("Samples with zero total count: \n")
            print(names(x$ninds)[which(x$ninds == 0)])
        }
    }
    if (any(x$specabund == 0)) {
        if (sum(x$specabund == 0) > cutoff) {						# FIX
            cat("Species with zero total count [1:", cutoff, 
                "]: \n")
            print(names(x$specabund)[which(x$specabund == 0)][1:cutoff])		# FIX
            cat("...\n")
        }
        else {
            cat("Species with zero total count: \n")
            print(names(x$specabund)[which(x$specabund == 0)])
        }
    }
    if (x$attributes == "both") {
        cat("Both attribute tables are attached: \n")
        print(x$attrib.matrix)
    }
    if (x$attributes == "samples.only") {
        cat("Only sample attribute table is attached: \n")
        print(x$attrib.matrix[1, ])
    }
    if (x$attributes == "species.only") {
        cat("Only species attribute table is attached: \n")
        print(x$attrib.matrix[2, ])
    }
    if (x$attributes != "species.only") {
        cat("Variables in the sample attribute table: \n")
        print(colnames(x$sample.attr))
    }
    if (x$attributes != "samples.only") {
        cat("Variables in the species attribute table: \n")
        print(colnames(x$species.attr))
    }
}


###
xorder <-					############ check.attrib elejére is !!!!!!!!!!!!!!!!!!
function (xc, which = c("samples", "species"), attrib, index = 0) 
{

#from
    if (class(xc) != "xcount" & class(xc) != "xclist") 
        stop("Object '", xc, "' is not of 'xcount' or 'xclist' class.")
    if (class(xc) == "xclist") xc <- xc$data[[1]]
#to

    check1 <- check.attrib(xc, which, attrib, index)$set.relation
    check2 <- check.attrib(xc, which, attrib, index)$duplicate
    if (sum(is.element(c("equal", "inclusion"), check1)) == 0) 
        stop("Set relation in 'check.attrib' result: ", check1, 
            ".\nMissing elements: ", check.attrib(xc, which, 
                attrib, index)$missing)
    if (index == 0) {
        attr.i <- rownames(attrib)
    }
    else {
        attr.i <- attrib[, index]
    }
    if (!is.null(check2)) 
        stop("Duplicates were found by 'check.attrib': ", check2, 
            ".")
    if (check1 == "equal") {
        attrib.out <- attrib[order(as.character(attr.i)), ]
        if (which == "species") 
            index.out <- as.character(colnames(xc$data))
        if (which == "samples") 
            index.out <- as.character(rownames(xc$data))
    }
    if (check1 == "inclusion") {
        if (which == "species") {
            exclude <- is.element(attr.i, colnames(xc$data))
            index.out <- as.character(colnames(xc$data))
        }
        if (which == "samples") {
            exclude <- is.element(attr.i, rownames(xc$data))
            index.out <- as.character(rownames(xc$data))
        }
        attrib.sub <- subset(attrib, exclude == TRUE)
        attrib.sub[] <- lapply(attrib.sub, function(x) x[drop = TRUE])
        if (index == 0) {
            attr.s <- rownames(attrib.sub)
        }
        else {
            attr.s <- attrib.sub[, index]
        }
        attrib.out <- attrib.sub[order(as.character(attr.s)), 
            ]
    }
    rownames(attrib.out) <- index.out
    attrib.out <- as.data.frame(attrib.out)
    out <- list(data = attrib.out, call = match.call(), which = which, 
        check.setrel = check1, na = sum(is.na(attrib.out)))
    class(out) <- "xorder"
    return(out)
}

###
check.attrib <-
function (xc, which = c("samples", "species"), attrib, index = 0) 
{

#from
    if (class(xc) != "xcount" & class(xc) != "xclist") 
        stop("Object '", xc, "' is not of 'xcount' or 'xclist' class.")
    if (class(xc) == "xclist") xc <- xc$data[[1]]
#to

#    if (class(xc) != "xcount") 
#        stop("Object '", xc, "' is not of 'xcount' class.")

    if (which == "species") {
        list.in.xc <- colnames(xc$data)
    }
    else {
        if (which == "samples") {
            list.in.xc <- rownames(xc$data)
        }
        else {
            stop("Undefined 'which' tag.")
        }
    }
    if (index == 0) {
        list.in.attr <- rownames(attrib)
    }
    else {
        list.in.attr <- as.character(attrib[, index])
    }
    if (length(union(list.in.xc, list.in.attr)) - nlevels(as.factor(list.in.xc)) - 
        nlevels(as.factor(list.in.attr)) == 0) {
        duplicate <- NULL
        set.relation <- "separate"
        missing <- list.in.xc
    }
    else {
        match <- subset(list.in.attr, is.element(list.in.attr, 
            list.in.xc))
        if (sum(apply(table(match, match) > 1, 1, sum)) == 0) 
            duplicate <- NULL
        else {
            duplicate <- subset(levels(as.factor(match)), apply(table(match, 
                match) > 1, 1, sum) != 0)
        }
        if (length(intersect(list.in.xc, list.in.attr)) == length(list.in.xc)) {
            missing <- NULL
            if (setequal(list.in.xc, list.in.attr) == TRUE) {
                set.relation <- "equal"
            }
            else {
                set.relation <- "inclusion"
            }
        }
        else {
            set.relation <- "intersect"
            missing <- setdiff(list.in.xc, list.in.attr)
        }
    }
    out <- list(call = match.call(), set.relation = set.relation, 
        duplicate = duplicate, missing = missing, na = sum(is.na(attrib)))
    return(out)
}

####################
###################
## animap fiz z.max --- ex mapmf !!!
zmap <-
function (xy, z, border=NULL, probs = seq(0, 1, 0.25), type = "heat", z.max = max(z), z.min=0, pch = 21, 
    cex = 1.5, zdots=TRUE, ...) 
{
    z.range <- z.max - z.min
    z.orig <- z
    if(zdots) z <- z[z.orig!=0] - z.min else z <- z - z.min
    if(zdots) xy2 <- xy[z.orig!=0,] else xy2 <-xy

    if(!zdots) plot(xy, type = "n", axes = FALSE, xlab = "", ylab = "", ...)
    else plot(xy, type = "p", pch=".", axes = FALSE, xlab = "", ylab = "", ...)

    if(!is.null(border)) lines(border)
    if (is.null(probs)) {
        if (type == "heat") 
            for (i in 1:length(z)) points(xy2[i, 1], xy2[i, 2], 
                bg = heat.colors(z.range)[(z.range - z[i] + 1)], 
                pch = pch, cex = cex, ...)
        if (type == "grey") 
            for (i in 1:length(z)) points(xy2[i, 1], xy2[i, 2], 
                bg = grey((z.range - z[i])/z.range), pch = pch, 
                cex = cex, ...)
    }
    else {
        zq <- as.factor(qdef(z, probs))
        nl <- nlevels(zq)
        if (type == "heat") 
            for (i in 1:nl) points(xy2[which(zq == levels(zq)[i]), 
                ], bg = heat.colors(nl)[(nl - i + 1)], pch = pch, 
                cex = cex, ...)
        if (type == "grey") 
            for (i in 1:nl) points(xy2[which(zq == levels(zq)[i]), 
                ], bg = grey(as.numeric(levels(zq)[(nl - i + 
                1)])), pch = pch, cex = cex, ...)
    }
}

#example
#xy <- cbind(rep(1:10,10), rep(1:10, each=10))
#z <- rpois(100,10)
#b <- rbind(c(1,10), c(10,10), c(10,1), c(1,1), c(1,10))
#zmap(xy,z,type="grey")
#zmap(xy,z,probs=NULL)
#zmap(xy,z,probs=NULL,border=b)
#zmap(xy,z,probs=NULL,border=b,z.min=min(z))
#
#zz <- rpois(100,1)
#zmap(xy,zz,probs=NULL,border=b,zdots=TRUE)


###
mfmap <- function(xc, xy, species=NULL, richness=TRUE, n=1, logscale=FALSE, draw=TRUE, ...)
{
if (class(xc) != "xcount" & class(xc) != "xclist" & class(xc) != "mefa" & class(xc) != "mflist") 
    stop("Class of object '", xc, "' is not appropriate.")
if (class(xc)=="mefa" | class(xc) == "mflist") xy <- xc$sample.attr[,xy]
if (class(xc) == "xclist" | class(xc) == "mflist") xc <- xc$data[[n]]

if (is.null(species)) if(richness) z <- xc$srichn else z <- xc$ninds
if (!is.null(species)){
    if(length(species) == 1){
        if(richness) z <- as.numeric(xc$data[,species] > 0)
        else z <- xc$data[,species]}
    if(length(species) > 1){
        if(richness) z <- marmat(xc$data[,species], "samples", "occur")
        else z <- marmat(xc$data[,species], "samples", "abund")}
    }
if(logscale) z <- log10(z+1)
if(draw) zmap(xy, z, ...)
xyz <- data.frame(xy,z)
colnames(xyz) <- c("x","y","z")
invisible(xyz)}

#mfmap(mf,c(2,3))
#mfmap(mf,c(2,3),probs=NULL)
#mfmap(mf,c(2,3),probs=NULL,species=1)
#mfmap(mf,c(2,3),probs=NULL,species=c(1:10))

animap <-
function(mfl, xy, species=NULL, richness=TRUE, logscale=FALSE, border=NULL, probs = seq(0, 1, 0.25), 
    type = "heat", z.min=0, pch = 21, cex = 1.5, zdots=TRUE,
    control=ani.control(interval=1), ...)
{
if (class(mfl) != "mflist") 
    stop("Object '", mfl, "' is not of class 'mflist'.")
require(animation)
if(!is.null(species) & length(species) == 1 & sum(mfl$data[[1]]$data[,species]) == 0)
    stop("Zero initial value for species.")
control <- checkargs(control, ...)

### set z.max for uniform scaling
maxv <- rep(0,mfl$length)
if(richness) {
    for(i in 1:mfl$length) maxv[i] <- max(mfl$data[[i]]$srichn)
    } else {
    for(i in 1:mfl$length) maxv[i] <- max(mfl$data[[i]]$ninds)}
z.max <- max(maxv)
if(logscale) z.max <- log10(z.max+1)

# start of plot for()
for(i in 1:mfl$length){

xyz <- mfmap(mfl$data[[i]], mfl$sample.attr[,xy], species=species, richness=richness, logscale=logscale, draw=FALSE)

zmap(xyz[,1:2], xyz$z, border=border, probs=probs, 
    type=type, z.max=z.max, z.min=z.min, pch=pch, cex=cex, zdots=zdots)

if (control$saveANI) 
    savePNG(n = i, width = control$width, height = control$height)
else Sys.sleep(control$interval)
}# end of plot for()
invisible(NULL)
}

### makes temporal accumulation statistics from sscount object,
### where segment is date (eg. year)

accumulate <- function(x){
tabl <- x$data[order(x$data$segment),]
nyr <- length(levels(tabl$segment))

nsamp <- newsamp <- nrecord <- nspec <- newspec <- rep(0, nyr)
first <- tabl[which(tabl$segment == levels(tabl$segment)[1]),]
first[] <- lapply(first, function(x) x[drop = TRUE])
nrecord[1] <- sum(first$count)
nspec[1] <- newspec[1] <- nrow(first)
oldlist1 <- first$species
oldlist2 <- first$sample

for (i in 2:nyr){
    second <- tabl[which(tabl$segment == levels(tabl$segment)[i]),]
    second[] <- lapply(second, function(x) x[drop = TRUE])
    nrecord[i] <- sum(second$count)
    newlist1 <- union(oldlist1, levels(second$species))
    nspec[i] <- nlevels(as.factor(newlist1))
    newspec[i] <- nspec[i] - nspec[(i-1)]
    oldlist1 <- newlist1
    newlist2 <- union(oldlist2, levels(second$sample))
    nsamp[i] <- nlevels(as.factor(newlist2))
    newsamp[i] <- nsamp[i] - nsamp[(i-1)]
    oldlist2 <- newlist2
    }

out <- data.frame(as.numeric(levels(tabl$segment)), nrecord, newsamp, newspec, cumsum(nrecord), nsamp, nspec)
colnames(out) <- c("segment", "newrecord", "newsamp", "newspec", "cumrecord", "cumsamp", "cumspec")
return(out)}

### check.ettrib-nál invisible(out) !!!
check.mefa <-
function(xc1, xc2, scan.zero = FALSE)
{
if(inherits(xc1,"mefa")) xc1 <- as.xcount(xc1)
if(inherits(xc1,"mefa")) xc2 <- as.xcount(xc2)
if(!inherits(xc1,"xcount") | !inherits(xc2,"xcount"))
    stop("Object classes are not appropriate.")

if(scan.zero) {
    spec1 <- colnames(xc1$data)[which(xc1$specoccur > 0)]
    spec2 <- colnames(xc2$data)[which(xc2$specoccur > 0)]
    samp1 <- rownames(xc1$data)[which(xc1$srichn > 0)]
    samp2 <- rownames(xc2$data)[which(xc2$srichn > 0)]
    if(length(which(xc1$specoccur == 0))==0) spec1z <- NULL else spec1z <- which(xc1$specoccur == 0)
    if(length(which(xc2$specoccur == 0))==0) spec2z <- NULL else spec2z <- which(xc2$specoccur == 0)
    if(length(which(xc1$srichn == 0))==0) samp1z <- NULL else samp1z <- which(xc1$srichn == 0)
    if(length(which(xc2$srichn == 0))==0) samp2z <- NULL else samp2z <- which(xc2$srichn == 0)
} else {
    spec1 <- colnames(xc1$data)
    spec2 <- colnames(xc2$data)
    samp1 <- rownames(xc1$data)
    samp2 <- rownames(xc2$data)
    spec1z <- NULL
    spec2z <- NULL
    samp1z <- NULL
    samp2z <- NULL
    }

names(spec1z) <- names(spec2z) <- names(samp1z) <- names(samp2z) <- NULL

if(setequal(spec1,spec2)) {
    both1 <- spec1
    first1 <- second1 <- NULL
    if(is.null(spec1z) & is.null(spec2z)) spec.equal <- TRUE else spec.equal <- FALSE
} else {
    if(length(intersect(spec1,spec2)) == 0) both1 <- NULL else both1 <- intersect(spec1,spec2)
    if(length(setdiff(spec1,spec2)) == 0) first1 <- NULL else first1 <- setdiff(spec1,spec2)
    if(length(setdiff(spec2,spec1)) == 0) second1 <- NULL else second1 <- setdiff(spec2,spec1)
    spec.equal <- FALSE
    }
if(setequal(samp1,samp2)) {
    both2 <- samp1
    first2 <- second2 <- NULL
    if(is.null(samp1z) & is.null(samp2z)) samp.equal <- TRUE else samp.equal <- FALSE
} else {
    if(length(intersect(samp1,samp2)) == 0) both2 <- NULL else both2 <- intersect(samp1,samp2)
    if(length(setdiff(samp1,samp2)) == 0) first2 <- NULL else first2 <- setdiff(samp1,samp2)
    if(length(setdiff(samp2,samp1)) == 0) second2 <- NULL else second2 <- setdiff(samp2,samp1)
    samp.equal <- FALSE
    }

if(is.null(first1))  excl1.spec <- spec1z else excl1.spec <- which(!is.element(colnames(xc1$data),both1))
if(is.null(second1)) excl2.spec <- spec2z else excl2.spec <- which(!is.element(colnames(xc2$data),both1))
if(is.null(first2))  excl1.samp <- samp1z else excl1.samp <- which(!is.element(rownames(xc1$data),both2))
if(is.null(second2)) excl2.samp <- samp2z else excl2.samp <- which(!is.element(rownames(xc2$data),both2))

species <- list(first=first1, second=second1, both=both1, eqal=spec.equal,
    excl1=excl1.spec, excl2=excl2.spec)
samples <- list(first=first2, second=second2, both=both2, eqal=samp.equal,
    excl1=excl1.samp, excl2=excl2.samp)
out <- list(call = match.call(), samples=samples, species=species, scan.zero=scan.zero)

invisible(out)
}
