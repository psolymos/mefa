# object class sscount

`sscount` <-
function(sstable, zc=NULL, fill=FALSE, digits=NULL){
if(ncol(as.data.frame(sstable)) < 3 | ncol(as.data.frame(sstable)) > 4) 
stop("Three or four columns are required.")

if(!is.null(zc)) if(sum(is.element(sstable[,2], zc)) == 0) {
cat("Samples with '", zc, "' were not detected, 'zc' is set to 'NULL'.\n")
zc <- NULL}
if(fill==TRUE) sstable <- fill.count(sstable)
if(sum(is.na(sstable)) != 0) stop("NA values were detected")

sample <- as.factor(sstable[,1])
species <- as.factor(sstable[,2])

if(ncol(sstable) == 3) {
if(!is.numeric(sstable[,3])) stop("Count must be numeric!")
segment <- rep("undefined", nrow(sstable))
count <- as.numeric(sstable[,3])
if(!is.null(zc)) {
    segment[which(sstable[,2] == zc)] <- NA
    count[which(sstable[,2] == zc)] <- 1
    }
segment <- as.factor(segment)
}

if(ncol(sstable) == 4) {
if(!is.numeric(sstable[,4])) stop("Count must be numeric!")
segment <- as.character(sstable[,3])
count <- as.numeric(sstable[,4])
if(!is.null(zc)) {
    segment[which(sstable[,2] == zc)] <- NA
    count[which(sstable[,2] == zc)] <- 1
    }
segment <- as.factor(segment)
}

frame <- data.frame(sample, species, segment, count)

if(is.null(digits)) if(sum(frame[,4]) != sum(trunc(frame[,4])))
    stop("count is not integer, use 'digits' argument")

if(!is.null(digits)) if(sum(frame[,4]) != sum(trunc(frame[,4]))) {
    frame[,4] <- round(frame[,4], digits = digits)
    } else {digits <- NULL}

frame[] <- lapply(frame, function(x) x[drop=TRUE])
colnames(frame) <- c("sample", "species", "segment", "count")
if(is.null(zc)) {nspecies <- nlevels(frame[,2])} else {nspecies <- nlevels(frame[,2]) - 1}

out <- list(
data = frame,
call = match.call(),
zc = zc,
digits = digits,
nsamples = as.numeric(nlevels(frame[,1])),
nspecies = as.numeric(nspecies),
segment.levels = levels(frame[,3])
)

class(out) <- "sscount"
return(out)}


# print method for object class sscount

print.sscount <- function(x, ...){
	cat("Object of class 'sscount'\n")
	cat("Call: ")
	print(x$call)
	cat("Data type: ")
		if(is.null(x$digits)) cat("count\n") else cat("measurement with ", x$digits, " digits\n")
	cat("Number of samples: ", x$nsamples, "\n")
	cat("Number of species: ", x$nspecies, "\n")
	if(is.null(x$zc)) cat("Zero count identifier: not used\n") else cat("Zero count identifier: ", x$zc, "\n")
	cat("Number of segments:", length(x$segment.levels), " with level")
	if(length(x$segment.levels) == 1) cat(":\n") else cat("s:\n")
	print(x$segment.levels)
	}

