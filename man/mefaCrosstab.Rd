\encoding{UTF-8}
\name{mefaCrosstab}
\alias{mefaCrosstab}
\title{Cross Tabulate an Object of Class 'stcs'}
\description{
The function makes cross tabulation of an object of class 'stcs'. Similar to the \code{\link{table}} function but handles specialities of the 'stcs' object class (i.e. removes 'pseudo species' created to indicate empty samples). This is used internally by the function \code{\link{mefa}} to make 3 dimensional representation of the data.
}
\usage{
mefaCrosstab(x, segment = FALSE, nested = FALSE, drop.zero = FALSE)
}
\arguments{
  \item{x}{an object of class 'stcs'.}
  \item{segment}{logical, if \code{TRUE}, the crosstabulation is made in 2 dimensions by the \code{samp} (sample) and \code{taxa} columns of the \code{x}. Otherwise, crosstabulation is made in 3 dimensions by the \code{samp}, \code{taxa} and \code{segm} (segment) columns.}
  \item{nested}{logical, if \code{FALSE} count values of each segments are independent, if \code{TRUE} crosstabulated matrices are summed up in a nested fashion from segment level 1 to the number of segments (it is indicated in the segment levels as well).}
  \item{drop.zero}{argument passed to the function \code{\link{stcs}}, indicating whether empty samples (with zero sum) should be removed prior to cross tabulation (\code{TRUE}), or not (\code{FALSE}).}
}
\details{
The cross tabulation of an object of class 'stcs' is made by the function \code{\link{xtabs}}, whic accepts non-integer values too.
}
\value{
It returs a matrix if \code{segment = FALSE} or a list with matrices for the segments if \code{segment = TRUE}.
}
\references{
\enc{S\'olymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'olymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \doi{10.18637/jss.v029.i08}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{ \code{\link{stcs}}, \code{\link{mefa}}, \code{\link{rep.data.frame}} }
\examples{
x <- data.frame(sample = LETTERS[c(1,1,2,2,3,4)],
    species = c(letters[c(5,5,5,6,7)], "zero.pseudo"),
    count = c(1,2,10,3,4,0), segment = letters[c(8,9,8,9,8,8)])
y <- stcs(x)
mefaCrosstab(y)
mefaCrosstab(y, segment = TRUE)
mefaCrosstab(y, segment = TRUE, nested = TRUE)
## Examples for non-integer data
y[1:5, 3] <- y[1:5, 3] + 0.123
mefaCrosstab(y)
mefaCrosstab(y, segment = TRUE)
}
\keyword{ manip }
