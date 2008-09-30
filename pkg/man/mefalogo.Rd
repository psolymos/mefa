\encoding{UTF-8}
\name{mefalogo}
\alias{mefalogo}
\title{Schematic Plot of an Object of Class 'mefa'}
\description{
Plots a shematic representation of and object of class 'mefa', or alternatively plots a colored logo of the \strong{mefa} package.
}
\usage{
mefalogo(type = c("cont", "fill"), labels = c("str", "r"),
adj=c(0,0), scale=c(1,1), new = TRUE, cex = 1, ann=FALSE, axes=FALSE,
xlim=c(0,10)*scale[1]+adj[1], ylim=c(0,10)*scale[2]+adj[2], \ldots)
}
\arguments{
  \item{type}{character, \code{"cont"} is for drawing only black contours, \code{"fill"} is for filled polygons.}
  \item{labels}{character, \code{"str"} for labelling according to the general structure of a 'mefa' object, \code{"r"} places a big \code{R} letter in the middle (for logo).}
  \item{adj}{numeric values to adjust the dimensions of the polygons.}
  \item{scale}{a numeric value to rescale the dimensions of the polygons.}
  \item{new}{logical, if a new plot (\code{TRUE}) should be drawn, or just adding the polygons and labels to an existing one (\code{FALSE}).}
  \item{cex}{numeric, argument for scaling the labels.}
  \item{ann}{logical, annotations of the plot (see \code{\link{par}}).}
  \item{axes}{logical, drawing axes or not (see \code{\link{par}}).}
  \item{xlim, ylim}{extreme values for the x and y axes.}
  \item{\ldots}{other arguments passed to the function \code{plot}, see \code{\link{par}}. Only effective if \code{new = TRUE}.}
}
\details{
This function was developed for depicting the general structure of the 'mefa' objects. The colored logo version can be used in presentations as well.
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
mefalogo()
mefalogo("fill", "r")
}
\keyword{ hplot }
