\encoding{UTF-8}
\name{read.abmi}
\alias{read.abmi}
\title{Read Text File}
\description{
A variant of the \code{\link{read.table}} function to handle some specifics of ABMI raw data formats.
}
\usage{
read.abmi(filename, dir = getwd(), remove.empty = TRUE,
header = TRUE, sep = ",", quote = "\"", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{character, name of the file to read.}
  \item{dir}{character, path of the directory where the file is located. By default, it is the current working directory. If specified othervise, it resets to the original value after the procedure.}
  \item{remove.empty}{logical. Because lines end with comma, last column is empty. This can be removed by this argument.}
  \item{header}{logical, if the forst row should be handled as header (for column names).}
  \item{sep}{character, separator used in the comma separated text file to separate entries of the rows.}
  \item{quote}{character, quotation marks that encapsulate values that should be treated as character. Note that the single quote is not used, because it occurrs in some species names.}
  \item{\dots}{other arguments passed to the function \code{\link{read.table}}.}
}
\value{
A data frame.
}
\references{\url{http://www.abmi.ca/}}
\author{\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}}
\seealso{\code{\link{read.table}}, \code{\link{read.csv}}}
\examples{
\dontrun{
## There is no 'real' ABMI data as an example
## but any data set downloaded from the ABMI
## site can be used (www.abmi.ca), e.g.:
x <- read.abmi("RAW_T_BirdCounts63451.csv")
x
}
}
\keyword{ IO }
