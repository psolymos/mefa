`inflate` <-
function(factors, count){

if(nrow(factors) != length(count)) stop("Number of dimensions must be equal!")
if(any(count < 0)) stop("Negative count value.")
if(any(count == 0)) stop("Zero count value")

infl.data <- array(data=NA, dim=c(ncol(factors), sum(count)))

factors <- as.matrix(factors)
j <- 1 # this goes to length(infl.data)

for (i in 1:length(count)){
		k <- j + count[i] - 1
		infl.data[ ,j:k] <- factors[i, ]
		j <- k + 1
}

out <- t(as.data.frame(infl.data))
colnames(out) <- colnames(factors)
rownames(out) <- c(1:nrow(out))
return(out)
}
