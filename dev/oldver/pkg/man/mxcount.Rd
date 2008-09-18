\name{mxcount}
\alias{mxcount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Merge Two Objects of Class 'xcount'}
\description{
The function merges two objects of class 'xcount'. Rows with zero total counts are handled internally by functions \code{\link{msscount}} and \code{\link{ttsscount}}.
}
\usage{
mxcount(xc1, xc2, segment = "unspecified")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xc1}{an object of class 'xcount'.}
  \item{xc2}{an object of class 'xcount'.}
  \item{segment}{arbitrary name of the segment (eg. all or mixed, see example). 
    When \code{segment} values of \code{xc1} and \code{xc2} are the same, \code{segment} argument specified here 
    has no effect and the original is retained. When \code{segment} values of \code{xc1} and \code{xc2} are different, 
    it is set to \code{"unspecified"} as default.}
}
\value{
A result is an object of class 'xcount'.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{xcount}}
}
\examples{
ss1 <- data.frame(
cbind(
c("sample1","sample1","sample2","sample2","sample3","sample4"),
c("species1","species1","species1","species2","species3","zero.count"),
c("male","female","male","female","male","male")
),
c(1, 2, 10, 3, 4, 1)
)
colnames(ss1) <- c("sample.id", "species.id", "gender", "catch")

ss2 <- data.frame(
cbind(
c("sample1","sample1","sample5","sample5"),
c("species1","species4","species1","species5"),
c("male","female","male","female")
),
c(3, 2, 1, 4)
)
colnames(ss2) <- c("sample.id", "species.id", "gender", "catch")

xc1 <- xcount(sscount(ss1, zc="zero.count"))
xc2 <- xcount(sscount(ss1, zc="zero.count"), 1)
xc3 <- xcount(sscount(ss2))

mxcount(xc1, xc2, "mixed")
mxcount(xc1, xc3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{utilities}
