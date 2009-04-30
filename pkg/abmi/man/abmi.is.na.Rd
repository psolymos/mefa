\encoding{UTF-8}
\name{abmi.is.na}
\alias{abmi.is.na}
\alias{abmi.convert.na}
\alias{abmi.na.omit}
\alias{abmi.na.exclude}
\title{Not Available / 'Missing' Values}
\description{
These function handle ABMI specific Na values.
}
\usage{
abmi.is.na(x, na.char = c("NA", "VNA", "DNC", "PNA", "SNI"))
abmi.convert.na(x, value = NA, na.char = c("NA", "VNA", "DNC", "PNA", "SNI"))
abmi.na.omit(x, value = NA, na.char = c("NA", "VNA", "DNC", "PNA", "SNI"))
abmi.na.exclude(x, value = NA, na.char = c("NA", "VNA", "DNC", "PNA", "SNI"))
}
\arguments{
  \item{x}{an object (vector, matrix or data frame).}
  \item{na.char}{character vector defining strings treated as NA.}
  \item{value}{value to return instead of \code{na.char} values.}
}
\details{
  ABMI has different types of NA values with different meaning:

  \describe{VNA}{\strong{V}ariable \strong{N}ot \strong{A}pplicable.
    Some ABMI data is collected in a nested manner. For example Tree Species is a parent variable. This
    variable has a number of child variables that are used to describe the parent variable in more detail
    (e.g., condition, DBH, decay stage). When the parent variable is recorded as “None,” child variables
    are no longer applied and are recorded as “VNA.” VNA is also used when the protocol calls for a
    modified sampling procedure based on site conditions (e.g., surface substrate protocol variant for
    hydric site conditions). The use of VNA implies that users of the data should not expect that any
    data could be present.}
  \describe{DNC}{\strong{D}id \strong{N}ot \strong{C}ollect.
    DNC is used to describe variables that should have been collected but were not. There are a
    number of reasons that data might not have been collected (e.g. staff oversight, equipment failure,
    safety concerns, environmental conditions, or time constraints). Regardless of the reason data was not
    collected, if under ideal conditions it should have been, the record in the data entry file reads DNC.
    The use of DNC implies that users should expect the data to be present -- though it is not.}
  \describe{PNA}{\strong{P}rotocol \strong{N}ot \strong{A}vailable.
    The ABMI's protocols were, and continue to be, implemented in a staged manner. As a result, the
    collection of many variables began in years subsequent to the start of the prototype or operational
    phases or where discontinued after a few years of trial. When a variable was not collected because the
    protocol had yet to be implemented by the ABMI (or was discontinued by the ABMI), the data entry
    record reads PNA. This is a global constraint to the data (i.e. a protocol was not implemented until
    2006, therefore, previous years cannot have this variable). PNA is to be used to describe the lack of
    data collection for entire years.}
  \describe{SNI}{\strong{S}pecies \strong{N}ot \strong{I}dentified.
    In various fields related to species identification, SNI is used to indicate that the organism was not
    identified to the species level. Some possible reasons that identification to the species level of
    resolution was not possible include, insufficient or deficient sample collected and lack of field time.}

The function \code{abmi.is.na} is similar of the primitive function \code{\link{is.na}}, evaluates if elements of
the input object are one of the cases of ABMI NA types.

\code{abmi.convert.na} convert ABMI NA types into NA values that can be identified by the \code{\link{is.na}} function.
It is advisable to use \code{abmi.convert.na} before using raw data to build e.g. lists.

\code{abmi.na.omit} and \code{abmi.na.exclude} work similarly to \code{\link{na.omit}} and \code{\link{na.exclude}} generic functions,
both remove NA cases (rows). Follow the links to see the differences.
}
\value{
  \code{abmi.is.na} returns an object with same class and dimension as the original, but containing logical values.
  TRUE for those elements marked NA and FALSE otherwise. \code{dim}, \code{dimnames} and \code{names} attributes are preserved. 

  Currently it does not support list objects.

  \code{abmi.convert.na} returns the original object with some elements turned into NA.

  \code{abmi.na.omit} and \code{abmi.na.exclude} returns the original objects with cases/rows corresponding to NA values deleted.
  Deleted cases are registered into the \code{"na.action"} attribute.
}
\references{\url{http://www.abmi.ca/}}
\author{\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}}
\seealso{\code{\link{na.omit}}, \code{\link{na.exclude}}, \code{\link{is.na}}}
\examples{
data(abmi.data)
abmi.is.na(abmi.data$sitecap[1:100,9])
abmi.is.na(abmi.data$sitecap[101:120,9:10])
abmi.convert.na(abmi.data$sitecap[1:100,9])
abmi.convert.na(abmi.data$sitecap[101:120,9:10])
abmi.na.omit(abmi.data$sitecap[1:100,9])
abmi.na.omit(abmi.data$sitecap[101:120,9:10])
abmi.na.exclude(abmi.data$sitecap[1:100,9])
abmi.na.exclude(abmi.data$sitecap[101:120,9:10])
}
\keyword{ manip }
