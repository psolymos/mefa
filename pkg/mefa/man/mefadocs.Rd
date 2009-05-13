\encoding{UTF-8}
\name{mefadocs}
\alias{mefadocs}
\title{Display Package Documentation}
\description{
  Display package documentation using \code{pager} or \code{pdfviewer}
  defined in \code{\link{options}}.
}
\usage{
mefadocs(doc = c("ChangeLog","SampleReport"))
}
\arguments{
  \item{doc}{The name of the document (partial match, case sensitive). }
}
\details{
\code{"ChangeLog"} containss version history, \code{"SampleReport"} shows an example LaTeX document how to use \code{\link{report.mefa}} to prepare \code{\link[utils]{Sweave}} reports based on faunistic data.
}
\author{\enc{P\'eter S\'olymos}{Peter Solymos} (\email{solymos@ualberta.ca}) adapted Jari Oksanen's \code{\link[vegan]{vegandocs}} function in the \strong{vegan} package.}
\examples{
\dontrun{
mefadocs("Change")
mefadocs("Sample")
}
}
\keyword{ documentation }
