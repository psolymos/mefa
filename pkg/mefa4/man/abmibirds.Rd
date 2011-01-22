\name{abmibirds}
\alias{abmibirds}
\docType{data}
\title{
ABMI Birds
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(abmibirds)}
\format{
  A data frame with 59341 observations on the following 21 variables.
  \describe{
    \item{\code{Rotation}}{a factor with levels \code{Prototype} \code{Rotation 1}}
    \item{\code{ABMI.Site}}{a numeric vector}
    \item{\code{Year}}{a numeric vector}
    \item{\code{Field.Date}}{a factor}
    \item{\code{Field.Crew.Member.s.}}{a factor}
    \item{\code{Identification.Date}}{a factor}
    \item{\code{Identification.Analyst}}{a factor }
    \item{\code{Point.Count.Station}}{a numeric vector}
    \item{\code{Wind.Conditions}}{a factor with levels \code{0} \code{1} \code{2} \code{3} \code{4} \code{5} \code{DNC}}
    \item{\code{Precipitation}}{a factor with levels \code{DNC} \code{Drizzle} \code{Fog} \code{None} \code{Rain}}
    \item{\code{Start.of.Point.Count..24.hour.clock.}}{a factor}
    \item{\code{End.of.Point.Count..24.hr.clock.}}{a factor}
    \item{\code{Scientific.Name}}{a factor}
    \item{\code{Taxonomic.Resolution}}{a factor with levels \code{Family} \code{Genus} \code{Species} \code{Subfamily} \code{VNA}}
    \item{\code{Unique.Taxonomic.Identification.Number}}{a factor}
    \item{\code{Time.First.Detected..seconds.}}{a factor}
    \item{\code{Interval.1..0.200.seconds.}}{a factor with levels \code{} \code{0} \code{1} \code{DNC} \code{PNA} \code{VNA}}
    \item{\code{Interval.2..201.400.seconds.}}{a factor with levels \code{} \code{0} \code{1} \code{DNC} \code{PNA} \code{VNA}}
    \item{\code{Interval.3..401.600.seconds.}}{a factor with levels \code{} \code{0} \code{1} \code{DNC} \code{PNA} \code{VNA}}
    \item{\code{Behaviour}}{a factor}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(abmibirds)
## maybe str(abmibirds) ; plot(abmibirds) ...
}
\keyword{datasets}
