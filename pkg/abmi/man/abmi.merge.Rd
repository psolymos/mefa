\encoding{UTF-8}
\name{abmi.merge}
\alias{abmi.merge}
\title{Merge Items of a List}
\description{
The function merge items of a list.
}
\usage{
abmi.merge(x, na.rm = FALSE, drop=FALSE)
}
\arguments{
  \item{x}{a list with named vectors (or one column data frames) of its element. Vectors can be of different lengths.}
  \item{na.rm}{logical, should NAs removed from the result?}
  \item{drop}{logical, should unused factor levels be dropped?}
}
\details{
This function can be useful, when vecors are aimed to combined into the same data frame, but has different orderings and lengths. The function takes care to match names (using the union of all names found).
}
\value{
Result is coerced to data frame with colnames corresponding to names attribute of \code{x} and rownames corresponding to union of names of elements of \code{x}.
}
\references{\url{http://www.abmi.ca/}}
\author{\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}}
\seealso{\code{\link{abmi.is.na}}, \code{\link{ithcase}}}
\examples{
## A simple case to see how it works
a <- 1:20
a
names(a) <- paste("x", 1:20, sep="")
set.seed(123)
## Create subsets and reorder it
b <- a[order(sample(1:20,15))]
c <- a[order(sample(1:20,10))]
## A list with vectors to be merged
li <- list(a = a, b = b, c = c)
## Note the lengths
sapply(li, length)
abmi.merge(li)
## Note NA removel effect
abmi.merge(li, na.rm = TRUE)
## ABMI example
data(abmi.data)
abmi.li <- list(
nutr.moist = ithcase(abmi.data$sitecap[,9], abmi.data$sitecap$ABMI.Site),
tree.modif = ithcase(abmi.data$sitecap[,10], abmi.data$sitecap$ABMI.Site),
stru.stage = ithcase(abmi.data$sitecap[,11], abmi.data$sitecap$ABMI.Site))
x1 <- abmi.merge(abmi.li)
x1
x2 <- abmi.merge(abmi.li, na.rm = TRUE)
x2
dim(x1)
dim(x2)
}
\keyword{ manip }
