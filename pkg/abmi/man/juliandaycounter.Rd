\encoding{UTF-8}
\name{juliandaycounter}
\alias{juliandaycounter}
\alias{hourcounter}
\title{Date and Time Conversion}
\description{
Conversion of character vectors numeric values for Julian day and hour.
}
\usage{
juliandaycounter(x, format = "\%d-\%b-\%y", yr.start = 8, yr.end = 9, na.rm = FALSE)
hourcounter(x, with.min = TRUE, with.sec = FALSE, na.rm = FALSE)
}
\arguments{
  \item{x}{a character vector.}
  \item{format}{data format as described on the help page of \code{\link{as.Date}}.}
  \item{yr.start}{start of year characters.}
  \item{yr.end}{start of year characters.}
  \item{with.min}{logical, is the input contain minutes?}
  \item{with.sec}{logical, is the input contain seconds?}
  \item{na.rm}{logical, should NA values removed?}
}
\details{
Date format for \code{juliandaycounter} is \code{"dd-mmm-yy"} with only the last two decimals of year (maximum sring length is 9),  by default.

Time format for \code{hourcounter} is \code{"hh:mm"} (or \code{"h:mm"} if hour is <10), by default. 24 hour clock format is used. The formatting of <10 minutes should follow \code{"hh:01"} and not \code{"hh:1"}. Time conversion is done by the \code{\link[chron]{times}} function in the \strong{chron} package.
}
\value{
A numeric vector in both cases. \code{juliandaycounter} returns number of Julian day since January 1 of the same year. \code{hourcounter} returns real values for hour of the day (fractions if not 0 minutes).
}
\references{\url{http://www.abmi.ca/}}
\author{\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}}
\note{
This is not a general solution for data and time conversions. Pre 2000 year are not handled, because data collection of ABMI started later.
}
\seealso{\code{\link{as.Date}}, \code{\link[chron]{times}}}
\examples{
data(abmi.data)
y <- abmi.data$birdcount$Field.Date
y <- unique(y)
y
juliandaycounter(y)
h <- abmi.data$birdcount$Start.of.Point.Count...24.hour.clock.
h <- h[sample(h, 100)]
h
hourcounter(h)
}
\keyword{ manip }
