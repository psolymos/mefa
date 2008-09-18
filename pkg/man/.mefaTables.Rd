\name{check.mefaTables}
\alias{.mefaTables}
\title{Subsetting and ordering matrix and data frame based on row or column names}
\description{
This function is called by the \code{\link{mefa}} function, but can be applied more generally for sobsetting and ordering.
}
\usage{
.mefaTables(xtab, dframe, margin, index = NULL, drop.index = FALSE, xtab.fixed = TRUE)
}
\arguments{
  \item{xtab}{a data matrix.}
  \item{dframe}{a data frame.}
  \item{margin}{margin of the \code{xtab} matrix to use row \code{margin = 1} or column names \code{margin = 2} in comparison with the rownames of \code{dframe}.}
  \item{index}{if \code{NULL} rownames are used directly, else it can be used to set which column of \code{dframe} should be used as rowname and for comparison.}
  \item{drop.index}{logical, should the \code{index} column of \code{dframe} be deleted (if \code{index != NULL}).}
  \item{xtab.fixed}{logical, if \code{TRUE} the \code{xtab} matrix is left intact, and \code{dframe} is subsetted accordingly. If \code{FALSE}, both \code{xtab} and \code{dframe} are subsetted returning values based on the the intersect of the compared names.}
}
\value{
Returns a list with elements \code{xtab} (matrix) and \code{dtab} (data frame), corresponding to the subsetted \code{xtab} and \code{dframe} data sets, respectively. Original column and row ordering in the input matrix is preserved.
}
\references{
Solymos P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
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
.mefaTables(x, f, 1)
## Now result is the intersect
rownames(f) <- letters[3:12]
.mefaTables(x, f, 1, xtab.fixed = FALSE)
}
\keyword{ manip }
