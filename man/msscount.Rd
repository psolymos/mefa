\name{msscount}
\alias{msscount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Merges Two Objects of Class 'sscount'}
\description{
The function merges two objects of class 'sscount'.
}
\usage{
msscount(ssc1, ssc2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ssc1}{an object of class 'sscount'.}
  \item{ssc2}{an object of class 'sscount'.}
}
\value{
A result is an object of class 'sscount'.
}
\note{
Merge operation terminates with error message if zero count identifier of one 'sscount' object is a valid species identifier for the other 'sscount' object, and vica versa.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{sscount}}, \code{\link{xcount}}
}
\examples{
ss <- data.frame(
cbind(
c("sample1","sample1","sample2","sample2","sample3","sample4"),
c("species1","species1","species1","species2","species3","zero.count"),
c("male","female","male","female","male","male")
),
c(1, 2, 10, 3, 4, 1)
)
colnames(ss) <- c("sample.id", "species.id", "gender", "catch")

ss2 <- ss[1:5,]

ssc1 <- sscount(ss, zc="zero.count")
ssc2 <- sscount(ss2)

msscount(ssc1, ssc1)
msscount(ssc1, ssc2)
msscount(ssc2, ssc2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{utilities}
