\name{inflate}
\alias{inflate}
\title{Inflate Data Frame According to Integer Vector}
\description{
This function inflates a data frame according to a column containing integers, by repeating each rows as many times as the integer value. This is an internal function used by the function \code{\link{xcount}} converting an object of 'sscount' into 'xcount'.
}
\usage{
inflate(factors, count)
}
\arguments{
  \item{factors}{a data frame or matrix used. Rows in \code{factors} are repeated as many times as the value of 
    respective value of \code{count}. Thus, number of rows in \code{factor} must equal the length of \code{count}.}
  \item{count}{a numeric (integer) vector with the same length as number of rows in \code{factors}.}
}
\value{
A matrix with columns as in \code{factors}, and rows according to sum of \code{count}.
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

inflate(ss[,1:3], ss[,4])
inflate(ss[,1:3], c(1:6))
inflate(ss[,1:3], rep(2, 6))
}
\keyword{manip}
\keyword{utilities}
