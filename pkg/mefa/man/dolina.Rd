\encoding{UTF-8}
\name{dolina}
\alias{dol.count}
\alias{dol.samp}
\alias{dol.taxa}
\docType{data}
\title{The Dolina Dataset}
\description{
Data were collected in dolines (sinkholes, karstic depressions) of Northeast Hungary.
}
\usage{
data(dol.count)
data(dol.samp)
data(dol.taxa)
}
\format{
  The \code{dol.count} data set is a data frame with 297 observations on the following 4 variables.
  \describe{
    \item{\code{samp}}{a factor with levels for samples.}
    \item{\code{taxa}}{a factor with levels for the species.}
    \item{\code{count}}{a numeric vector with count data.}
    \item{\code{segm}}{a factor with levels for segments.}
  }

  The \code{dol.samp} data set is a data frame with 24 observations on the following 2 variables.
  \describe{
    \item{\code{microhab}}{a factor with levels \code{dead.wood} \code{litter} \code{live.wood} \code{rock} for microhabitats.}
    \item{\code{method}}{a factor with levels \code{time} \code{quadrat} for the sampling method applied.}
  }

  The \code{dol.taxa} data set is a data frame with 121 observations on the following 4 variables.
  \describe{
    \item{\code{species}}{a factor with levels the species names.}
    \item{\code{author}}{a factor with levels for authors of the species and description dates (accents are removed).}
    \item{\code{familia}}{a factor with levels for family level classification.}
    \item{\code{size}}{a numeric vector for major shell dimension of the adult specimen.}
  }
}
\details{
The data sets are results of the 'DOLINA 2007' expedition (Vilisics et al. 2008). Species characteristics are based on Kerney et al. (1983). The full dataset (\enc{S\'olymos}{Solymos} &  Kemencei 2008) from which this example was derived is available at the Dataverse Network (\url{hdl:1902.1/12060}) with the code to make the subsetting.
}
\source{
\enc{S\'olymos}{Solymos}, P.,  Kemencei, Z. (2008) Methodological study data set of land snails from the Dolina 2007 project. Available at the Dataverse Network: \url{hdl:1902.1/12060}.

\enc{S\'olymos}{Solymos}, P., Kemencei, Z., \enc{P\'all}{Pall}-Gergely, B., Farkas, R., Vilisics, F., Hornung, E. (2009) Does shell accumulation matter in micro scale land snail surveys? \emph{Malacologia}, in press.

Vilisics, F., Nagy, A., \enc{S\'olymos}{Solymos}, P., Farkas, R., Kemencei, Z., \enc{P\'all}{Pall}-Gergely, B., Kisfali, M., Hornung, E. (2008) Data on the terrestrial isopoda fauna of the \enc{Als\'o}{Also}-hegy, Aggtelek National Park, Hungary. \emph{Folia Faunistica Slovaca} \bold{13}, 9--12. \url{http://zoology.fns.uniba.sk/ffs/interface/00053-Vilisics-et-al-2008.pdf}.

Kerney, M. P., Cameron, R. A. D., Jungbluth, J. H. (1983) \emph{Die Landschnecken Nord- und Mitteleuropas}. P. Parey, Hamburg-Berlin, 384 p.
}
\examples{
data(dol.count)
str(dol.count)
data(dol.samp)
str(dol.samp)
data(dol.taxa)
str(dol.taxa)
}
\keyword{datasets}
