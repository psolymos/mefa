\name{mefa4-package}
\alias{mefa4-package}
\alias{mefa4}
\docType{package}
\title{
Multivariate Data Handling with S4 Classes and Sparse Matrices
}
\description{
An S4 update of the 'mefa' package
using sparse matrices for enhanced efficiency.
}
\details{
An S4 update of the 'mefa' package
using sparse matrices for enhanced efficiency.
Sparse array-like obejects are supported via
lists of sparse matrices.

Main functions: \code{\link{Xtab}}, \code{\link{Mefa}}.

Accessor and replacement functions: \code{\link{xtab}}, 
\code{\link{samp}}, \code{\link{taxa}}.

Methods: \code{\link{mbind}}, \code{\link{groupSums}}, 
\code{\link{groupMeans}}.

Coercion methods and virtual classes defined for
cross compatibitity with the \code{\link[mefa]{mefa}} package.
S4 object classes are described in \code{\link{Mefa}} help page.

The vignette \code{vignette("mefa4")} gives an overview of the package,
gives a comparison of S3 and S4 object classes, and presents a performance
review.
}
\author{
Peter Solymos

Maintainer: Peter Solymos <solymos@ualberta.ca>
}
\references{
Solymos P. (2008) mefa: an R 
package for handling and reporting count data. 
\emph{Community Ecology} \bold{9}, 125--127.

Solymos P. (2009) Processing ecological 
data in R with the mefa package. 
\emph{Journal of Statistical Software} 
\bold{29(8)}, 1--28. 
\url{http://www.jstatsoft.org/v29/i08/}

\url{http://mefa.r-forge.r-project.org/}
}
\keyword{ package }
\seealso{
S3 classes: \code{\link[mefa]{mefa}}
}
\examples{
\dontrun{
vignette("mefa4")
}
}
