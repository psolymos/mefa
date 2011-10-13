\name{groupSums}
\alias{groupSums}
\alias{groupSums,matrix-method}
\alias{groupSums,sparseMatrix-method}
\alias{groupSums,Mefa-method}
\alias{groupMeans}
\alias{groupMeans,matrix-method}
\alias{groupMeans,sparseMatrix-method}
\alias{groupMeans,Mefa-method}
\title{
Compute Summary Statistics of Data Subsets
}
\description{
Compute summary statistics (sums, means) of data subsets.
}
\usage{
groupSums(object, ...)
\S4method{groupSums}{matrix}(object, MARGIN, by, na.rm = FALSE, ...)
\S4method{groupSums}{sparseMatrix}(object, MARGIN, by, na.rm = FALSE, ...)
\S4method{groupSums}{Mefa}(object, MARGIN, by, replace, na.rm = FALSE, ...)

groupMeans(object, ...)
\S4method{groupMeans}{matrix}(object, MARGIN, by, na.rm = FALSE, ...)
\S4method{groupMeans}{sparseMatrix}(object, MARGIN, by, na.rm = FALSE, ...)
\S4method{groupMeans}{Mefa}(object, MARGIN, by, replace, na.rm = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
  an object.
}
  \item{MARGIN}{
numeric, \code{1} indicates rows are to be summed/averaged, \code{2} indicates columns are to be summed/averaged.
\code{c(1, 2)} is not yet implemented, but can be calculated calling the function twice.
}
  \item{by}{a vector of grouping elements corresponding to dimensions of \code{object} and \code{MARGIN}.
}
  \item{replace}{a data frame to be used when applying the method on a  \code{"Mefa"} object. The attribute table
  corresponding to \code{MARGIN} is dropped (\code{NULL}), \code{replace}ment table can be specified via this argument.
}
  \item{na.rm}{logical. Should missing values be removed?
  Sum is calculated by zeroing out \code{NA} values,
  mean is calculated as dividing the sum by the number of non-\code{NA} values when collapsing.
}
  \item{\dots}{
other argument, currently not implemented.
}
}
\details{
The method sums/averages cells in a matrix. The functions behind these methods use sparse matrices,
so calculations can be more efficient compared to using \code{\link{aggregate}}.
}
\value{
An object similar to the input one.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link{rowSums}}, \code{\link{rowMeans}}, \code{\link{colSums}}, \code{\link{colMeans}}

Standard \code{\link{aggregate}} in package \pkg{stats}

\code{\link[mefa]{aggregate.mefa}} in package \pkg{mefa} for S3 \code{"mefa"} objects.
}
\examples{
x <- data.frame(
    sample = paste("Sample", c(1,1,2,2,3,4), sep="."),
    species = c(paste("Species", c(1,1,1,2,3), sep="."),  "zero.pseudo"),
    count = c(1,2,10,3,4,0))
samp <- data.frame(samples=levels(x$sample), var1=1:2)
taxa <- data.frame(specnames=levels(x$species), var2=c("b","a"))
rownames(samp) <- samp$samples
rownames(taxa) <- taxa$specnames
x2 <- Xtab(count ~ sample + species, x, cdrop=FALSE,rdrop=TRUE)
x5 <- Mefa(x2, samp, taxa, join="inner")

groupSums(as.matrix(x2), 1, c(1,1,2))
groupSums(as.matrix(x2), 2, c(1,1,2,2))
groupSums(x2, 1, c(1,1,2))
groupSums(x2, 2, c(1,1,2,2))
groupSums(x5, 1, c(1,1,2))
groupSums(x5, 2, c(1,1,2,2))

groupMeans(as.matrix(x2), 1, c(1,1,2))
groupMeans(as.matrix(x2), 2, c(1,1,2,2))
groupMeans(x2, 1, c(1,1,2))
groupMeans(x2, 2, c(1,1,2,2))
groupMeans(x5, 1, c(1,1,2))
groupMeans(x5, 2, c(1,1,2,2))
}
\keyword{ manip }
\keyword{ methods }
