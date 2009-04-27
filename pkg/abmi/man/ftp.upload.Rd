\name{ftp.upload}
\Rdversion{1.1}
\alias{ftp.upload}
\title{
Upload File via FTP Protocol
}
\usage{
ftp.upload(dumplist, ..., ftp, file = "results", ext = if (binary) "Rdata" else "txt", binary = FALSE, dir = getwd())
}
\arguments{
  \item{dumplist}{
A list with R objects or a character string naming an R object to write.
}
  \item{\dots}{
Names of the objects to be saved other than \code{dumplist}.
}
  \item{ftp}{
FTP address to be used, e.g. \code{"ftp://user:pass@ftp.org/"}.
}
  \item{file}{
Character string to use for file name.
}
  \item{ext}{
Character string to use as extension of the file.
}
  \item{binary}{
Logical, if \code{TRUE} the memory image is written, or the dumped text version otherwise.
}
  \item{dir}{
Optional character string where to write the local copy of the file. Orininal directory is restored.
}
}
\value{
Writes a binary or dumped text representation of R objects via FTP protocol. As a side effect, a local copy is created on the hard drive that has to be deleted manually.
}
\references{
\url{http://www.abmi.ca}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
The function is built on the \code{\link[RCurl]{ftpUpload}} of the \pkg{RCurl} package.
}
\examples{
\dontrun{
x <- "Helo World!"
ftp.upload(x, ftp = "ftp://user:pass@ftp.org/")
}
}
\keyword{ IO }

