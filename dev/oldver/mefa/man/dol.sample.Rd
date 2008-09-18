\name{dol.sample}
\alias{dol.sample}
\docType{data}
\title{Land Snail Data from a Dolina}
\description{
Data file contains count data of land snails in a dolina at Also-hegy, Aggtelek National Park, North Hungary.
}
\usage{
data(dol.count)
data(dol.sample)
}
\format{
  \code{dol.count} is a data frame with 125 observations on the following 4 variables (samples, species, segment, count).
    \code{NA} values refer to the last non-\code{NA} valueof the given column.
  \code{dol.sample} is a data frame with 16 observations (samples) on the 7 variables
    (sample, microhabitat, replicate, aspect, depth zone ofdolina, litter moisture and litter thickness).
}
\seealso{
  \code{\link{dol.count}}
}
\examples{
data(dol.count)
data(dol.sample)
}
\keyword{datasets}
