\encoding{UTF-8}
\name{mefaTables}
\alias{mefaTables}
\title{Subsetting and Ordering of Related Data Tables}
\description{
This function is called by the \code{\link{mefa}} function to subset the community data matrix and the related data frames, but can be applied more generally for subsetting and ordering.
}
\usage{
mefaTables(xtab, dframe, margin, index = NULL,
drop.index = FALSE, xtab.fixed = TRUE)
}
\arguments{
  \item{xtab}{a data matrix.}
  \item{dframe}{a data frame.}
  \item{margin}{margin of the \code{xtab} matrix to use row (\code{margin = 1}) or column (\code{margin = 2}) names in comparison with the rownames of \code{dframe}.}
  \item{index}{if \code{NULL} rownames are used directly, else it can be used to set which column of \code{dframe} should be used as rowname and for comparison.}
  \item{drop.index}{logical, should the \code{index} column of \code{dframe} be deleted (if \code{index != NULL}).}
  \item{xtab.fixed}{logical, if \code{TRUE} the \code{xtab} matrix is left intact, and \code{dframe} is subsetted accordingly. If \code{FALSE}, both \code{xtab} and \code{dframe} are subsetted, and returned values are based on the the intersect of the compared names.}
}
\value{
Returns a list with elements \code{xtab} (matrix) and \code{dtab} (data frame), corresponding to the subsetted \code{xtab} and \code{dframe} data sets, respectively. Original column and row orderings in the input matrix \code{xtab} are preserved.
}
\references{
\enc{S\'olymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'olymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \url{http://www.jstatsoft.org/v29/i08/}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{mefa}}}
\examples{
x <- matrix(rpois(20,1), 5, 4)
## Note the reverse alphabetical names
rownames(x) <- letters[5:1]
x
f <- data.frame(matrix(rnorm(30), 10, 3))
rownames(f) <- letters[1:10]
f
## Reverse alphabetical names preserved
mefaTables(x, f, 1)
## Now result is the intersect
rownames(f) <- letters[3:12]
mefaTables(x, f, 1, xtab.fixed = FALSE)
}
\keyword{ manip }
