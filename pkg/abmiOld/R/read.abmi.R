## reads data files of ABMI, removing last empty column as well
read.abmi <-
function(filename, dir=getwd(), remove.empty=TRUE, header=TRUE, sep=",", quote="\"", ...)
{
    oldir <- getwd()
    setwd(dir)
    x <- read.table(filename, header=header, sep=sep, quote=quote, ...)
    if (remove.empty) x <- x[ , -ncol(x)]
    setwd(oldir)
    return(x)
}
