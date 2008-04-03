\name{accumulate}
\alias{accumulate}
\title{Data Accumulation Trends from 'sscount' Object}
\description{
This function can be used to assess accumulation trends based on object of class 'sscount', if \code{segment} column 
of the 'sscount' object can be ordered (e.g. collecting dates).
}
\usage{
accumulate(x)
}
\arguments{
  \item{x}{an object of class 'sscount'.}
}
\value{
A data frame with \code{nlevels(x$data$segment)} number of rows and 7 columns.
  \item{segment}{ordered levels of \code{x$data$segment}.}
  \item{newrecord}{number of new records (or number of individuals, based on \code{x$data$count}).}
  \item{newsamp}{number of new samples in subsequent segment levels (records are treated as segments within samples).}
  \item{newspec}{number of new species in subsequent segment levels.}
  \item{cumrecord}{cumulative number of new records.}
  \item{cumsamp}{cumulative number of new samples in subsequent segment levels (records are treated as segments within samples).}
  \item{cumspec}{cumulative number of new species in subsequent segment levels.}
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{sscount}}
}
\examples{
ssc <- sscount(data.frame(
    LETTERS[rpois(20,1)+1], # samples
    letters[rpois(20,2)+1], # species
    rpois(20,10)+2000,      # years of observetions
    rep(1,20)))             # single counts for each rows

ssc$data

accumulate(ssc)
}
\keyword{ manip }
