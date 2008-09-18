\name{fill.count}
\alias{fill.count}
\title{Replace 'NA' Values}
\description{
This function replaces \code{NA} values according to the last non-\code{NA} value in a given column of a table.
}
\usage{
fill.count(table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{a matrix or data frame.}
}
\details{
It is easy and convinient way to fill up values in a data table with non-redundant entries, eg. leaving empty spaces just like on a sheet of a notebook. By this function, such objects can be directly used in subsequent operations, eg. by function \code{\link{sscount}}.
}
\value{
A result is a matrix or data frame according to class of \code{table}.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{sscount}}
}
\examples{
### Example 1: simple atrificial data

ss <- data.frame(
cbind(
c("sample1",NA,"sample2",NA,"sample3","sample4"),
c("species1",NA,"species1","species2","species3","zero.count"),
c("male","female","male","female","male","male")
),
c(1, 2, 10, 3, 4, 1)
)
colnames(ss) <- c("sample.id", "species.id", "gender", "catch")

ss

fill.count(ss)

sscount(fill.count(ss), zc="zero.count") ## equivalent as
sscount(ss, zc="zero.count", fill=TRUE)

### Example 2: field data of the dolina

\dontrun{
data(dol.count)

dol.count

fill.count(dol.count)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{utilities}
