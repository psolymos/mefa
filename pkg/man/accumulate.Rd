\name{accumulate}
\alias{accumulate}
\title{Accumulation Trends from !sscount! Objects}
\description{
This function can be used to summarise accumulation trends on object class "scount", if \code{segment} column 
of of the 'sscount' object can be ordered (e.g. time).
}
\usage{
accumulate(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class 'scount'.}
}
\value{
A data frame with \code{nlevels(x$data$segment)} number of rows and 5 columns.
\code{segment} contains ordered levels of \code{x$data$segment}, \code{newrecord} is the number of new records (or number of individuals, based on \code{x$data$count}), \code{newspec} is the number of new species in subsequent segment levels, \code{cumrecord} is the cumulative number of new records, \code{cumspec} is the cumulative number of new species in subsequent segment levels.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{sscount}}
}
\examples{
ssc <- sscount(data.frame(
    LETTERS[rpois(20,1)+1], # samples
    letters[rpois(20,2)+1], # species
    rpois(20,10)+2000,      # years of observetion
    rep(1,20)))             # 1s for single counts

ssc$data

accumulate(ssc)
}
\keyword{ manip }
