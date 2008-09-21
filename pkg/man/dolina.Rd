\name{dolina}
\alias{DOLINAcounts}
\alias{DOLINAsamples}
\alias{DOLINAtaxa}
\docType{data}
\title{Land snails in dolines}
\description{
Data were collected in dolines (karstic depressions) of Northeast Hungary.
}
\usage{
data(DOLINAcounts)
data(DOLINAsamples)
data(DOLINAtaxa)
}
\format{
  The \code{DOLINAcounts} data set is a data frame with 697 observations on the following 4 variables.
  \describe{
    \item{\code{samp}}{a factor with levels for samples.}
    \item{\code{taxa}}{a factor with levels for the species.}
    \item{\code{count}}{a numeric vector with count data.}
    \item{\code{segm}}{a factor with levels for segments.}
  }

  The \code{DOLINAsamples} data set is a data frame with 64 observations on the following 10 variables.
  \describe{
    \item{\code{subsample}}{a factor with levels for subsamples (corresponding levels of \code{count$samp}).}
    \item{\code{sample}}{a factor with levels for samples}
    \item{\code{dolina}}{a numeric vector referring to the two sampled dolines.}
    \item{\code{microhab}}{a factor with levels \code{dead.wood} \code{litter} \code{live.wood} \code{rock} for microhabitats.}
    \item{\code{replicate}}{a numeric vector for replicate within microhabitat.}
    \item{\code{method}}{a factor with levels \code{time} \code{quadrat} for the sampling method applied.}
    \item{\code{aspect}}{a factor with levels \code{eastern} \code{flat} \code{northern} \code{southern} \code{western} describing the aspect of the sample location within the doline.}
    \item{\code{stratum}}{a factor with levels \code{bottom} \code{edge} \code{middle} \code{outside} describing the position of the sample location relative to the dolina's morphology.}
    \item{\code{lmoist}}{a numeric vector for litter moisture (1--4 categories from dry to wet).}
    \item{\code{ldepth}}{a numeric vector for litter depth (cm).}
  }

  The \code{DOLINAtaxa} data set is a data frame with 121 observations on the following 8 variables.
  \describe{
    \item{\code{id}}{a factor with levels for four letter acronyms of the species names.}
    \item{\code{species}}{a factor with levels the species names.}
    \item{\code{author}}{a factor with levels for authors of the species and description dates (accents are removed).}
    \item{\code{size}}{a numeric vector for major shell dimension of the adult specimen.}
    \item{\code{familia}}{a factor with levels for family level classification.}
    \item{\code{subfamilia}}{a factor with levels subfamily level classification.}
    \item{\code{genus}}{a factor with levels generic level classification.}
    \item{\code{order}}{a numeric vector for the taxonomic order of the species.}
  }
}
\details{
The data sets are results of the 'Dolina 2007' field study. Species characteristics are based on Kerney et al. (1983).
}
\source{
Paper for count and sample characteristics in preparation. Please cite according to the mefa package, see \code{citation("mefa")}.

Kerney, M.P., Cameron, R.A.D. & Jungbluth, J.H., 1983. \emph{Die Landschnecken Nord- und Mitteleuropas}. P. Parey, Hamburg-Berlin, 384 p.
}
\examples{
data(DOLINAcounts)
str(DOLINAcounts)
data(DOLINAsamples)
str(DOLINAsamples)
data(DOLINAtaxa)
str(DOLINAtaxa)
}
\keyword{datasets}
