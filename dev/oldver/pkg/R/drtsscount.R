`drtsscount` <-
function(table, sample, segment="unspecified", digits=NULL){
#if(!is.integer(as.integer(as.matrix(table)))) stop("Count must be integer!")
if(length(sample) != dim(table)[1]) stop("Dimensions are not the same.")
if(sum(is.na(table)) != 0) stop("NA values were detected")
if(sum(is.na(sample)) != 0) stop("NA values were detected")
table <- as.matrix(table)

if(is.null(digits)) if(sum(table) != sum(trunc(table)))
    stop("count is not integer, use 'digits' argument")

if(!is.null(digits)) if(sum(table) != sum(trunc(table))) {
    table <- round(table, digits = digits)
    } else {digits <- NULL}

sample <- as.factor(sample)
zc <- NULL
excl <- subset(table, apply(table, 1, sum) ==0)
sample.excl <- subset(sample, apply(table, 1, sum) == 0)
rexcl <- dim(excl)[1]
sample <- subset(sample, apply(table, 1, sum) > 0)
table <- subset(table, apply(table, 1, sum) > 0)
rows.out <- sum(table > 0)
data.out <- as.data.frame(array(data = NA, dim = c(rows.out + rexcl, 4), dimnames = NULL))
r <- 1
for(i in 1:dim(table)[2]) {# species
table.sub <- subset(table[,i], table[,i] > 0)
sample.sub <- subset(sample, table[,i] > 0)
for(j in 1:length(sample.sub)){ #samples within species
data.out[r,1] <- as.character(sample.sub[j])
data.out[r,2] <- colnames(table)[i]
data.out[r,3] <- as.character(segment)
data.out[r,4] <- as.numeric(table.sub[j])
r <- r + 1}}
if(rexcl != 0){
for(z in 1:rexcl){
data.out[rows.out+z,1] <- as.character(sample.excl[z])
data.out[rows.out+z,2] <- "zero.count"
data.out[rows.out+z,3] <- segment
data.out[rows.out+z,4] <- 1}
zc <- "zero.count"
cat(dim(excl)[1], "Rows with 0 total count were found and flagged as 'zero.count'.\n")}
data.fin <- data.frame(
as.factor(data.out[,1]),
as.factor(data.out[,2]),
as.factor(data.out[,3]),
as.numeric(data.out[,4]))

if(is.null(zc)) {
    nspecies <- nlevels(data.fin[,2])
    } else {
    nspecies <- nlevels(data.fin[,2]) - 1}

out <- sscount(data.fin, zc=zc, fill=FALSE, digits=digits)
out$call <- match.call()

return(out)}

