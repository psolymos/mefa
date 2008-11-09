\encoding{UTF-8}
\name{plot.mefa}
\alias{plot.mefa}
\alias{boxplot.mefa}
\alias{image.mefa}
\alias{plot.stcs}
\alias{boxplot.stcs}
\alias{image.stcs}
\title{Plotting Methods for Object of Class 'mefa' and 'stcs'}
\description{
Various methods for plotting objects of class 'mefa'.}
\usage{
\method{plot}{mefa}(x, stat = 1:4, type = c("bar", "rank"),
trafo = c("none", "log", "ratio"), ylab = NULL, xlab = NULL, show = TRUE, \dots)
\method{boxplot}{mefa}(x, stat = 1:4, ylab = NULL, xlab = NULL, show = TRUE, \dots)
\method{image}{mefa}(x, segm=NULL, trafo=c("none", "log", "bins", "prab"), 
probs = seq(0, 1, 0.05), ordering = TRUE, reverse = TRUE, ylab = NULL, 
xlab = NULL, show = TRUE, \dots)
\method{plot}{stcs}(x, stat = 1:4, type = c("bar", "rank"),
trafo = c("none", "log", "ratio"), ylab = NULL, xlab = NULL, show = TRUE, \dots)
\method{boxplot}{stcs}(x, stat = 1:4, ylab = NULL, xlab = NULL, show = TRUE, \dots)
\method{image}{stcs}(x, segm=NULL, trafo=c("none", "log", "bins", "prab"), 
probs = seq(0, 1, 0.05), ordering = TRUE, reverse = TRUE, ylab = NULL, 
xlab = NULL, show = TRUE, \dots)
}
\arguments{
  \item{x}{an object of class 'mefa' or 'stcs'.}
  \item{stat}{numeric, to determine which characteristic to plot. \code{1}: number of species in samples (default), \code{2}: total number of indiwiduals in samples, \code{3}: number of occurrences per taxa, \code{4} number of individuals per taxa.}
  \item{type}{character, \code{"bar"} produces barchart (default), \code{"rank"} ranked curves based on the characteristic defined by \code{stat}.}
  \item{trafo}{character, transformation of the plotted variable. \code{"none"}: no transformation (default), \code{"log"}: logarithmic transformation (base 10), \code{"ratio"}: normalizes values by the maximum and rescales to the [0, 1] interval (useful for plotting multiple rank abundance curves), \code{"bins"}: recodes the values according to quantiles based on \code{probs}, \code{"prab"}: presence absence transformation of count data.}
  \item{ylab, xlab}{character to overwrite default label for the y and x axes. If \code{NULL}, than default labels are returned on the plot.}
\item{segm}{if \code{NULL} the \code{x$xtab} matrix is used for plotting. Otherways, this defines the segment (one element in \code{x$segm}) for plotting (can be numeric or character with the name of the segment).}
  \item{probs}{numeric vector of probabilities with values in [0, 1] (passed internally to \code{\link{qvector}}).}
  \item{ordering}{logical, if \code{TRUE} (default) the samples-by-taxa matrix is ordered by row and columns sums, if \code{FALSE} row and columns are not rearranged.}
  \item{reverse}{logical, if the values to plot should be reversed (\code{TRUE}, default, original zero values are lightly, while higher values are strongly coloured) or not (\code{FALSE}). This is related to \code{col} argument of the generic function \code{\link{image}}. Currently, \code{\link{heat.colors}} is the default color scheme.}
  \item{show}{logical, produce a plot (\code{TRUE}) or not (\code{FALSE}). \code{FALSE} can be useful, if the returned plotted values are reused (e.g. the matrix returned invisibly \code{image} can be used by \code{\link{filled.contour}}, or multiple values are used in one plot).}
  \item{\dots}{further arguments to pass to plotting functions. See especially \code{zlim} and \code{col} arguments of the generic function \code{\link{image}}, and arguments for the generic function \code{\link{boxplot}}.}
}
\details{
\code{plot} returns barchart, or ranked curve of summary statistics (number of species, individuals in samples, number of occurrences or abundence of taxa) based on the \code{x$xtab} matrix of the 'mefa' objects. These walues are basically returned by \code{\link{summary.mefa}}.

\code{boxplot} returns box-and-whiskers plots for the summary statistics based on matrices for each segments in \code{x$segm}.

\code{image} creates a grid of colored rectangles with colors corresponding to the values in the segment defined by the argument \code{segm}. If \code{ordering = TRUE}, the ordering of the segment will be based on the \code{x$xtab} matrix and not on the matrix for the segment itself. This is due to better comparison among segments.

All graphical display methods for objects of class 'stcs' are based on the conversion of the object into 'mefa', and than the respective plotting method is applied.
}
\value{
All methods produce a plot if \code{show = TRUE}, and return the plotted values if \code{show = FALSE} without plotting them.
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{image}}, \code{\link{plot}}, \code{\link{boxplot}}, \code{\link{filled.contour}}, \code{\link{par}}, \code{\link{heat.colors}}, \code{\link{heat.colors}}, \code{\link{topo.colors}}, \code{\link{terrain.colors}}, \code{\link{rainbow}}}
\examples{
data(DOLINAcounts, DOLINAsamples, DOLINAtaxa)
x <- mefa(stcs(DOLINAcounts), DOLINAsamples, DOLINAtaxa)
## Histograms
par(mfrow=c(2,2))
plot(x, 1)
plot(x, 2)
plot(x, 3)
plot(x, 4)
par(mfrow=c(1,1))
## Ranked curves
par(mfrow=c(2,2))
plot(x, 1, type="rank")
plot(x, 2, type="rank")
plot(x, 3, type="rank")
plot(x, 4, type="rank")
par(mfrow=c(1,1))
## Boxplot for segments
par(mfrow=c(2,2))
boxplot(x, 1)
boxplot(x, 2)
boxplot(x, 3)
boxplot(x, 4)
par(mfrow=c(1,1))
## Image (levelplot)
## comparing all and the segments
par(mfrow=c(1,3))
image(x, trafo = "bins", main = "All segments")
image(x, segm = 1, trafo = "bins", main = dimnames(x)$segm[1])
image(x, segm = 2, trafo = "bins", main = dimnames(x)$segm[2])
par(mfrow=c(1,1))
}
\keyword{ hplot }
\keyword{ methods }
