\name{plot.mefa}
\alias{plot.mefa}
\alias{boxplot.mefa}
\alias{image.mefa}
\title{Plotting methods for objects of class 'mefa'}
\description{
Various methods for plotting objects of class 'mefa'.}
\usage{
\method{plot}{mefa}(x, which=1:4, type=c("hist", "rank"),
trafo=c("none", "log", "ratio"), ylab=NULL, xlab=NULL, show=TRUE, \dots)
\method{boxplot}{mefa}(x, which = 1:4, ylab = NULL, xlab = NULL, \dots)
\method{image}{mefa}(x, segm=NULL, trafo=c("none", "log", "bins", "prab"), 
probs = seq(0, 1, 0.05), ordering=TRUE, reverse=TRUE, ylab=NULL, 
xlab=NULL, show=TRUE, \dots)
}
\arguments{
  \item{x}{an object of class 'mefa'.}
  \item{which}{numeric, to determine what characteristic to plot. \code{1}: number of species in samples (default), \code{2}: total number of indiwiduals in samples, \code{3}: number of occurrences per taxa, \code{4} number of individuals per taxa.}
  \item{type}{character, \code{"hist"} produces histogram (default), \code{"rank"} ranked curves based on the characteristic defined by \code{which}.}
  \item{trafo}{character, transformation of the plotted variable. \code{"none"}: no transdormation (default), \code{"log"}: logarithmic transformation (base 10), \code{"ratio"}: divides the values by the maximum of them and rescales to the [0, 1] interval (useful for plotting multiple relative abundance curves), \code{"bins"}: recodes the values accurding to quantiles based on \code{probs}, \code{"prab"}: presence absence transformation of count data.}
  \item{ylab}{character to owerwrive default label for the y axis. If \code{NULL}, than default labels are returned on the plot.}
  \item{xlab}{character to owerwrive default label for the x axis. If \code{NULL}, than default labels are returned on the plot.}
\item{segm}{if \code{NULL} the \code{x$xtab} matrix is used for plotting. Otherways, this defines the segment (element in \code{x$segm}) for plotting (can be numeric or character with the name of the segment).}
  \item{probs}{numeric vector of probabilities with values in code{[0, 1]} (passed internally to \code{\link{qvector}}).}
  \item{ordering}{logical, if \code{TRUE} (default) the samples-by-taxa matrix is ordered by row and columns sums, if \code{FALSE} row and columns are left as are.}
  \item{reverse}{logical, if the values to plot are reversed (\code{TRUE}, default, original zero values are lightly, while higher values are strongly coloured) or not (\code{FALSE}. This is a matter of taste, and related to \code{col} in the generic function \code{\link{image}}. Currently, \code{\link{heat.colors}} is the default color scheme.}
  \item{show}{logical, produce a plot (\code{TRUE}) or not (\code{FALSE}). (\code{FALSE}) can be useful, if the plotted values (returned invisibly) are recicled (e.g. the matrix returned invisibly \code{image} can be used by \code{\link{filled.contour}}).}
  \item{\dots}{further arguments to pass to plotting functions. See especially \code{zlim} and \code{col} arguments of the generic function \code{\link{image}}.}
}
\details{
\code{plot} returns histogram, or ranked curve of summary statistics (number of species, individuals in samples, number of occurrences or abundence of taxa) based on the \code{x$xtab} matrix of the 'mefa' objects. These walues are basically returned by the \code{summary} method.

\code{boxplot} returns box-and-whiskers plots according to the summary statistics based on matrics for each segments (\code{x$segm}).

\code{image} creates a grid of colored or gray-scale rectangles with colors corresponding to the values in the segment defined by the argument \code{segm}. If \code{ordering=TRUE}, the ordering of the segment will be based on the \code{x$xtab} matrix and not on the matrix for the semnet itself. This is due to better comparison among segments.
}
\value{
All methods return the plotted values invisibly.
}
\references{
Solymos P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{image}}, \code{\link{plot}}, \code{\link{boxplot}}, \code{\link{filled.contour}}, \code{\link{par}}, \code{\link{heat.colors}}, \code{\link{heat.colors}}, \code{\link{topo.colors}}, \code{\link{terrain.colors}}, \code{\link{rainbow}}}
\examples{



}
\keyword{ hplot }
\keyword{ methods }
