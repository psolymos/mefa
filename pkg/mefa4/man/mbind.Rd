\name{mbind}
\alias{mbind}
\alias{mbind,matrix,matrix-method}
\alias{mbind,sparseMatrix,sparseMatrix-method}
\alias{mbind,Mefa,Mefa-method}
\alias{mbind2}
\alias{mbind2,matrix,matrix-method}
\alias{mbind2,sparseMatrix,sparseMatrix-method}
\title{
Combine R Objects by Rows and Columns
}
\description{
Combine R objects by rows and columns.
}
\usage{
mbind(x, y, fill, ...)
mbind2(x, y, fill, ...)
}
\arguments{
  \item{x, y}{
objects to combine, see Details.
}
  \item{fill}{
numeric value or \code{NA} (default) to fill up outer set 
(not part of union) of dimnames.
}
  \item{\dots}{
other argument, not used.
}
}
\details{
\code{x} and \code{y} are combined in a left join manner, 
meaning that all the elements in
\code{x} are retained, and only non-overlapping elements in \code{y} are used.
Elements of the returning object that are not part of \code{x} 
and \code{y} (outer set) are filled up with \code{fill}.

If relational table in \code{x} is \code{NULL}, 
corresponding values from same table
of \code{y} are used.

\code{mbind2} combines \code{x} and \code{y} so that inner set is
calculated as sum of corresponding elements from \code{x} and \code{y} 
(unlike in \code{mbind} with a left join manner).
}
\value{
An object similar to the input one.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link{rbind}}, \code{\link{cbind}} 

\code{\link{rBind}}, \code{\link{cBind}} for sparse matrices in 
\pkg{Matrix} package
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
xy <- mbind(Mefa(x,sampx),Mefa(y,sampy,taxay))
unclass(xy)

mbind2(x,y)
mbind2(as(x,"sparseMatrix"),as(y,"sparseMatrix"))
xtab(xy) <- mbind2(x, y)
unclass(xy)
}
\keyword{ methods }
\keyword{ manip }% __ONLY ONE__ keyword per line
