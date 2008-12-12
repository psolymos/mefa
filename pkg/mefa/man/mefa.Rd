\encoding{UTF-8}
\name{mefa}
\alias{mefa-package}
\alias{mefa}
\alias{is.mefa}
\alias{as.mefa}
\alias{print.mefa}
\alias{summary.mefa}
\alias{print.summary.mefa}
\alias{dim.mefa}
\alias{dimnames.mefa}
\title{Create an Object of Class 'mefa'}
\description{
An object of class 'mefa' is a compendium of a crosstabulated taxa-by-samples (count) data, and optionally (count) data for segments, and linked tables for samples and taxa. The 'mefa' term stand for an acronym of '\strong{me}ta\strong{fa}unistics', indicationg that data processing is a critical and often time consuming step before data analysis. The 'mefa' package aims to help in this respect.
}
\usage{
mefa(xtab, samp = NULL, taxa = NULL, id.samp = NULL, id.taxa = NULL,
segment = TRUE, nested = FALSE, drop.zero = FALSE, drop.index = FALSE,
xtab.fixed = TRUE)
\method{as}{mefa}(x, samp, taxa, \ldots)
\method{is}{mefa}(x)
\method{print}{mefa}(x, nlist = 10, \ldots)
\method{summary}{mefa}(object, nlist = 10, \ldots)
\method{print}{summary.mefa}(x, nlist, \ldots)
\method{dim}{mefa}(x)
\method{dimnames}{mefa}(x)
}
\arguments{
  \item{xtab}{data. Can be an object of class 'stcs', or a matrix or data frame. Can be count (integer) or non-integer.}
  \item{samp}{a data frame containing rows for samples, or \code{NULL}. See argument \code{xtab.fixed} and details for sample matching options.}
  \item{taxa}{a data frame containing rows for taxa, or \code{NULL}. See argument \code{xtab.fixed} and details for taxa matching options.}
  \item{id.samp}{\code{NULL} if sample names for matching are the rownames of \code{samp}, otherwise, the index refers to the column of \code{samp} where the names are located.}
  \item{id.taxa}{\code{NULL} if taxa names for matching are the rownames of \code{taxa}, otherwise, the index refers to the column of \code{taxa} where the names are located.}
  \item{segment}{logical, whether information in the segments column of the 'stcs' object should be used (\code{TRUE}, default). It has no effect when \code{xtab} is matrix or data frame.}
  \item{nested}{logical, whether segments should be nested within each other from the first to the last (\code{TRUE}) or not (\code{FALSE}, default).}
  \item{drop.zero}{logical, if \code{TRUE} empty samples are removed from the result (can be useful before multivariate analyses). Otherwise, empty rows are left intact (\code{FALSE}, default).}
  \item{drop.index}{logical, whether columns of the samples and taxa tables containing the names used for matching should be removed (\code{TRUE}) to reduce redundancy or not (\code{FALSE}, default). This argument has effect only if \code{id.samp} and \code{id.taxa} are not \code{NULL}.}
  \item{xtab.fixed}{logical, if \code{TRUE} (default) the \code{samp} and \code{taxa} tables are subsetted and ordered according to the names of \code{xtab}. If \code{FALSE}, \code{xtab} is also subsetted according to the intersect of the names. Row and column ordering is determined by \code{xtab} in either cases.}
  \item{x, object}{an object of class 'mefa'.}
  \item{nlist}{number of elements in the list of segment names to print. List of segment names is truncated at 10 by default.}
  \item{\dots}{other arguments passed to functions.}
}
\details{
Main goals of the \code{mefa} function are to (1) convert a long formatted object of 'stcs' into a crosstabulated and optionally segmented 'mefa' object and (2) link this crosstabulation with tables of samples and taxa. The segments can be nested within each other. This feature can be useful, if segments represent e.g. subsequent sampling periods (years) and the the aim is to detect effects of data accumulation over time.

A mefa object can be viewed as a project oriented compendium of the data. It contains 3 dimensional (samples, taxa, segments) representation of the count data and data tables for samples and taxa. Segments contain information on some particular internal division of the data (sub populations, method specific partitions, repeated measures), but it has technical significance. Thus data tables for segments are not supported.

Input values may contain non-integer values as well.

If the input \code{xtab} is a matrix, dimnames are necessary if either \code{samp} or \code{taxa} tables are provided. In other cases, dimnames are not necessary. In this way, simple statistics can be returned for the matrix.

The print method returns basic information, instead of a long structural representation.

The summary contains statistics calculated from the 'mefa' object (species richness, number of individuals, occupancy, abundance, total sum, matrix fill, etc., see 'Value' section). The list can be recalled by e.g. \code{unclass(summary(x))}, elements of the list e.g. \code{summary(x)$s.rich}.

Methods for extracting, subsetting, plotting and reporting 'mefa' objects are discussed elsewhere (see links in 'See also' section). On how the 'mefa' objects can be used in further analyses, see 'Examples' and the vignette (\code{vignette("mefa")}).

When the \code{as.mefa} function is used with a 'mefa' object as argumentum, samples and taxa tables are used when \code{samp} and \code{taxa} atguments are missing. If the aim is to redefine these tables, those also can be given.
}
\value{
The \code{mefa} function returns an object of class 'mefa'. It is a list with 5 elements:

  \item{call}{the function call.}
  \item{xtab}{crosstabulated count data, rows are samples, columns are taxa.}
  \item{segm}{list of matrices if segments are used, otherwise \code{NULL}.}
  \item{samp}{a data frame for sample related data (rows correspond to samples) if provided, otherwise \code{NULL}.}
  \item{taxa}{a data frame for taxa related data (rows correspond to taxa) if provided, otherwise \code{NULL}.}


The \code{summary.mefa} function returns a list invisibly, with elements:
  \code{s.rich}: vector with species richness values for the samples,
  \code{s.abu}: vector with number of individuals values for the samples,
  \code{t.occ}: number of sites occupied by each species,
  \code{t.abu}: abundances of each species,
  \code{ntot}: total number of individuals,
  \code{mfill}: matrix fill,
  \code{nsamp}: number of samples,
  \code{ntaxa}: number of taxa,
  \code{nsegm}: number of segments,
  \code{segment}: vector of segment names,
  \code{call}: the function call from the 'mefa' object,
  \code{nested, drop.zero, xtab.fixed}: attributes of the 'mefa' object.

The method \code{dim} returns a vector of length 3 with values for number of samples, number of taxa and number of segments in the 'mefa' object. The third value is 1 in cases, when \code{s$segm} is \code{NULL} (because it is 1 undefined segment essentially identical.to the matrix \code{x$xtab}, thus no need for a replicate).

The \code{dimnames} method returns a list of three character vectors for names of samples, taxa and segments. These can be \code{NULL} as well.
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\note{
The \code{mefa} function alone can be used instead of the combination of object classes \code{xcount} and \code{xorder} used in older (< 2.0) versions of the \strong{mefa} package. Further it is also extended by previously undocumented features.
}
\seealso{
Further methods are discussed on seperate help pages: see \code{\link{[.mefa}} and \code{\link{aggregate.mefa}} for extracting and aggregating the data, \code{\link{melt.mefa}} for redefining segments or melting data into long format, \code{\link{report.mefa}} for generating report into file, and \code{\link{plot.mefa}} \code{\link{boxplot.mefa}} and \code{\link{image.mefa}} for graphical display.

The simple 2 dimensional wide arrangement of long formatted data can be done by the function \code{\link[reshape]{reshape}} in the \strong{reshape} package of Hadley Wickham, by the \code{\link[simba]{mama}} function in the \strong{simba} package by  Gerald Jurasinski, and by the function \code{\link[labdsv]{matrify}} in the \strong{labdsv} package of David W. Roberts. These are, however, not useful either for 3 dimensional (segmented) crosstabulation (see \code{\link{table}} and \code{\link{xtabs}} functions), or linking data tables for sample and taxa with the count data.
}
\examples{
data(dol.count, dol.samp, dol.taxa)
## Input is stcs
x1 <- mefa(stcs(dol.count))
x1
## Input is matrix
x2 <- mefa(x1$xtab)
x2
## Attach data frame for samples
x3 <- mefa(stcs(dol.count), dol.samp)
x3
## Attach data frame for samples and taxa
x4 <- mefa(stcs(dol.count), dol.samp, dol.taxa)
x4
## Methods
## (chapter 'See also' provides
## links for further methods)
summary(x4)
## Descriptives inside the summary
unclass(summary(x4))
## Testing mefa objects
is(x4, "mefa")
## Removing empty samples
as.mefa(x4, drop.zero = TRUE)
## Dimensions
dim(x4)
## Dimnames
dimnames(x4)
## Simple examples how to use mefa objects in analyis
## GLM on sample abundances
mod <- glm(summary(x4)$s.abu ~ .^2,
    data = x4$samp, family = quasipoisson)
summary(mod)
## See the demo and vignette for more examples
#demo(mefa)
#vignette("mefa")
## Cluster analysis of community matrix
x5 <- aggregate(x4, c("method", "microhab"))
h <- hclust(dist(x5$xtab))
plot(h)
}
\keyword{ classes }
\keyword{ manip }
\keyword{ methods }
