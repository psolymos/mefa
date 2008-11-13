## separates interaction vector
separateint <-
function(x, step = 0, spacer = 1)
{
x <- as.character(x)
n <- nchar(x)
first <- substr(x, 1, n-step-1-spacer)
second <- substr(x, n-step, n)
out <- cbind(first, second)
colnames(out) <- c("first", "second")
rownames(out) <- x
return(out)
}
