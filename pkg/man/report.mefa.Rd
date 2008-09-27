\encoding{UTF-8}
\name{report.mefa}
\alias{report.mefa}
\alias{report}
\title{Write Report of an Object of Class 'mefa' into File}
\description{
The data of the 'mefa' object is written into a file in a standard format used by most local journals for publishing distribution data of organisms. The aim is to provide an exporting facility that can directly be used for generating reports, parts of research papers, or supplementary materials.
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
  \item{by.taxa}{logical, whether the main sectioning should be made according to taxa (\code{TRUE}, default) or samples (\code{FALSE}, currently not implemented).}
  \item{samp.var}{variables in the samples table (\code{x$samp}) to be used in the report. Variables appear in the order given here.}
  \item{drop.redundant}{logical, whether redundant elements should be dropped (an integer up to \code{length(samp.var)-1}) or not (\code{NULL}, default).}
  \item{collapse}{logical, if \code{TRUE} samples with identical values in \code{samp.var} for a given species are collapsed to avoid redundancy.}
  \item{taxa.name}{column in the taxa table (\code{x$taxa}) where the taxa names are stored. By default (\code{NULL}) \code{dimnames(x)$taxa} is used.}
  \item{author.name}{column in the taxa table (\code{x$taxa}) where the authors' names and dates of descriptions are stored. By default it is not used (\code{NULL}). If specified, these follow the species names in the report.}
  \item{taxa.order}{column in the taxa table (\code{x$taxa}) where the variable for ordering is stored. By default (\code{NULL}) \code{taxa.name} is used for ordering.}
  \item{grouping}{logical, whether all records of a given species should be collapsed into single paragraph without grouping (\code{FALSE}, default), or grouped into separate paragraphs (\code{TRUE}).}
  \item{tex}{logical, whether to generate formatted LaTeX output (\code{TRUE}) or plain text file (\code{FALSE}).}
  \item{binary}{logical, if \code{TRUE} the result will contains count data.}
  \item{tex.control}{a list with LaTex formatting options (if \code{tex = TRUE}). The \code{ital.taxa} controls italicised taxa names, \code{noindent} controls indentation of paragraphs, \code{bold.sect} controls boldface type of paragraph openings, \code{bold.1st} controls the boldface type of first elements of each record of a given species, \code{vspace1} controls the space between species blocks, \code{vspace2} controls the space between paragraphs of the same species (if \code{gouping = FALSE}).}
  \item{sep}{character vector of length 7. The 1st separates values in \code{samp.var}, 2nd is placed between values of \code{samp.var} if redundant elements were dropped, 3rd is the opening character before count data, 4th stands after the segment name, 5th separates values of segments, 6th is the closing character after the count data, 7th separates the records.}
  \item{outdir}{character, full path of the directory where the file should be written (working directory is then restored). If \code{NULL}, the current working directoty is used.}
  \item{\ldots}{further arguments potentially passed (currently there are none).}
}
\details{
 The resulting file can be recicled by copy-and-pasting into a document (if \code{tex = FALSE}) or be included into a LaTeX report (e.g. via the package \code{Sweave}, see \code{mefadocs("SampleReport")}). Contrary to the many arguments, the default values do not need much modifications in most of the cases (see examples).
}
\value{
Writes a file into the working (or a specified) directory.
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\note{
The \code{by.taxa = FALSE} option is not implemented.
}
\seealso{\code{\link{mefa}}, \code{\link{write}}}
\examples{
data(DOLINAcounts, DOLINAsamples, DOLINAtaxa)
x <- mefa(stcs(DOLINAcounts), DOLINAsamples, DOLINAtaxa)
\dontrun{
## Plain text
## Count values from x$xtab
report(x, "report-all.txt")
## Count values for each segments
report(x, "report-segm-all.txt",
    samp.var = c("method","microhab","stratum"), segment = TRUE)
## Count values for segments 2 and 3
report(x, "report.-segm-23.txt",
    samp.var = c("method","microhab","stratum"), segment = TRUE, n = 2:3)
## LaTeX formatting
report(x, "report-all.tex", tex=TRUE)
## For how to include into a TeX or Rnw file, see:
mefadocs("SampleReport")
}
}
\keyword{ methods }
\keyword{ IO }
