\name{mbind}
\alias{mbind}
\alias{mbind,matrix,matrix-method}
\alias{mbind,sparseMatrix,sparseMatrix-method}
\alias{mbind,Mefa,Mefa-method}
\title{
Combine R Objects by Rows and Columns
}
\description{
Combine R objects by rows and columns
}
\usage{
mbind(x, y, fill, ...)
}
\arguments{
  \item{x, y}{
objects to combine, see Details.
}
  \item{fill}{
numeric value or \code{NA} (default) to fill up outer set (not part of union) of dimnames.
}
  \item{\dots}{
other argument, not used.
}
}
\details{
\code{x} and \code{y} are combined in a left join manner, meaning that all the elements in
\code{x} are retained, and only non-overlapping elements in \code{y} are used.
Elements of the returning object that are not part of \code{x} and \code{y} (outer set)
are filled up with \code{fill}.
}
\value{
An object similar to the input one.
}
\author{
Modified by Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link{rbind}}, \code{\link{cbind}} 

\code{\link{rBind}}, \code{\link{cBind}} for sparse matrices in \pkg{Matrix} package
}
\examples{
x=matrix(1:4,2,2)
rownames(x) <- c("a","b")
colnames(x) <- c("A","B")
y=matrix(11:14,2,2)
rownames(y) <- c("b","c")
colnames(y) <- c("B","C")

sampx <- data.frame(x1=1:2, x2=2:1)
rownames(sampx) <- rownames(x)
sampy <- data.frame(x1=3:4, x3=10:11)
rownames(sampy) <- rownames(y)
taxay <- data.frame(x1=1:2, x2=2:1)
rownames(taxay) <- colnames(y)
taxax <- NULL

mbind(x,y)
mbind(as(x,"sparseMatrix"),as(y,"sparseMatrix"))
mbind(Mefa(x,sampx),Mefa(y,sampy,taxay))
}
\keyword{ methods }
\keyword{ manip }% __ONLY ONE__ keyword per line
