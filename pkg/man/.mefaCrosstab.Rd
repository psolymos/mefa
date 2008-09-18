\name{crosstab}
\alias{.mefaCrosstab}
\title{Crosstabulation of an object of class 'stsc'}
\description{
The function makes crosstabulation of an object of class 'stsc'. Similar to the \code{\link{table}} function but handles specialties of the 'stsc' object class (i.e. removes 'pseudo species' created to indicate empty samples). This is used internally by the function \code{\link{mefa}} to make 3 dimensional representation of the data.
}
\usage{
.mefaCrosstab(x, segment = FALSE, nested = FALSE, drop.zero = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class 'stsc'.}
  \item{segment}{logical, if \code{TRUE}, the crosstabulation is made in 2 dimensions by the \code{samp} (sample) and \code{taxa} columns of the \code{x}. Otherwise, crosstabulation is made in 3 dimensions by the \code{samp}, \code{taxa} and \code{segm} (segment) columns.}
  \item{nested}{logical, if \code{FALSE} count values of each segments are independent, if \code{TRUE} crosstabulated matrices are summes in a nested fashion from segment level 1 to the number of segments (it is indicated in the segment levels as well).}
  \item{drop.zero}{argument passed to the function \code{\link{stsc}}, indicatinh whether empty samples (with zero sum) should be removed prior to crosstabulation (\code{TRUE}), or not (\code{FALSE}).}
}
\details{
The cross tabulation of an object of class 'stsc' is made after the inflation of the data table by the \code{expand = TRUE} argument of the function \code{\link{stsc}}. The inflation is made by the function \code{\link{inflate}}.
}
\value{
It returs a matrix if \code{segment = FALSE} or a list with matrices for the segments if \code{segment = TRUE}.
}
\references{
Solymos P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}, 
}
\seealso{ \code{\link{stsc}}, \code{\link{mefa}}, \code{\link{inflate}} }
\examples{
x <- data.frame(sample = LETTERS[c(1,1,2,2,3,4)],
    species = c(letters[c(5,5,5,6,7)], "zero.pseudo"),
    count = c(1,2,10,3,4,0), segment = letters[c(8,9,8,9,8,8)])
y <- stsc(x)
.mefaCrosstab(y)
.mefaCrosstab(y, segment = TRUE)
.mefaCrosstab(y, segment = TRUE, nested = TRUE)
}
\keyword{ manip }
