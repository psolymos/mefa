\name{nonDuplicated}
\alias{nonDuplicated}
\title{
Non Duplicated Rows in Data Frame
}
\description{
Subset a data frame using non duplicated elemnts in a vector.
}
\usage{
nonDuplicated(x, y, change.rownames = FALSE)
}
\arguments{
  \item{x}{
a data frame.
}
  \item{y}{
a vector. It can be a name of a column in \code{x} without quotes.
}
  \item{change.rownames}{
if original rownames of \code{x} are to be replaced by unique 
non duplicated values of \code{y}.
}
}
\details{
This function is handy to keep only one set of duplicated data that is common
in long formatted database files.
}
\value{
A data frame.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link{duplicated}}
}
\examples{
data(abmibirds)
x <- nonDuplicated(abmibirds, abmibirds$ABMI.Site, TRUE)
## or equivalently
#x <- nonDuplicated(abmibirds, ABMI.Site, TRUE)
dim(abmibirds)
dim(x)
length(unique(abmibirds$ABMI.Site))
}
\keyword{ manip }
