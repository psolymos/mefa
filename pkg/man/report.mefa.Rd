\name{report.mefa}
\alias{report.mefa}
\alias{report}
\title{Write report into file based on an object of class 'mefa'}
\description{
The data of the 'mefa' object is written into a file in a standard format used by most local journals for publishing distribution data of living organisms. The aim is to provide an export facility that can directly be used for generating reports, parts of research papers, or supplementary materials.
}
\usage{
report(x, \ldots)
\method{report}{mefa}(x, filename, segment = FALSE, n = NULL,
    by.taxa = TRUE, samp.var = NULL, drop.redundant = NULL,
    collapse = TRUE, taxa.name = NULL, author.name = NULL,
    taxa.order = NULL, grouping = FALSE, tex = FALSE, binary = FALSE,
    tex.control = list(ital.taxa = TRUE, noindent = TRUE, 
    bold.sect = TRUE, bold.1st = TRUE, vspace1 = 0.5, vspace2 = 0.2),
    sep = c(",", ":", "(", ":", ",", ")", ";"), outdir = NULL, \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class 'mefa'.}
  \item{filename}{character, the name of the file to be written into the working directory or a directory defined by the \code{outdir} argument.}
  \item{segment}{logical, if \code{NULL} (default) segments are not distinguished in the output. If \code{TRUE} all (\code{n = NULL}) or some of the segments are used and distinguished.}
  \item{n}{segments to be used. If \code{NULL} all segments are used, if not \code{NULL}, give names of the segments to be used (only if \code{segment = TRUE}).}
  \item{by.taxa}{logical, whether the main sectioning shold be made according to taxa (\code{TRUE}, default) or samples (\code{FALSE}, currently unavailable).}
  \item{samp.var}{variables in the samples table (\code{x$samp}) to be used in the report. Variables will be used in the order given here.}
  \item{drop.redundant}{logical, whether redundant elements should be dropped (an integer up to \code{length(samp.var)-1}) or not (\code{NULL}, default).}
  \item{collapse}{logical, whether all data of a given species should be collapsed into single paragraph (\code{TRUE}, defauld) or written into separate paragraphs (\code{FALSE}).}
  \item{taxa.name}{column in the taxa table (\code{x$taxa}) where the taxa names are stored. By default (\code{NULL}) \code{dimnames(x)$taxa} is used.}
  \item{author.name}{column in the taxa table (\code{x$taxa}) where the author names and dates of description are stored. By default it is not used (\code{NULL}).}
  \item{taxa.order}{column in the taxa table (\code{x$taxa}) where the variable for ordering is stored. By default (\code{NULL}) \code{taxa.name} is used for ordering.}
  \item{grouping}{logical, whether .}
  \item{tex}{}
  \item{binary}{}
  \item{tex.control}{}
  \item{sep}{}
  \item{outdir}{character, full path of the directory where the file should be written (working directory is then restored). If \code{NULL}, the current rowking directoty is used.}
  \item{\ldots}{further arguments potentially passed (currently there are none).}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{
Solymos P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\note{
The \code{by.taxa = FALSE} option is not implemented.
}
\seealso{\code{\link{mefa}}, \code{\link{write}}}
\examples{

}
\keyword{ methods }
\keyword{ IO }
