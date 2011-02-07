\name{samp}
\alias{xtab}
\alias{samp}
\alias{taxa}
\alias{xtab<-}
\alias{samp<-}
\alias{taxa<-}
\alias{[}
\alias{xtab,Mefa-method}
\alias{samp,Mefa-method}
\alias{taxa,Mefa-method}
\alias{xtab<-,Mefa,MefaMatrix-method}
\alias{samp<-,Mefa,MefaDataFrame-method}
\alias{taxa<-,Mefa,MefaDataFrame-method}
\alias{[,Mefa,ANY,ANY,ANY-method}
\alias{samp,mefa-method}
\alias{taxa,mefa-method}
\alias{xtab,mefa-method}
\title{
Accessing and Replacing Parts of 'Mefa' Objects
}
\description{
Methods to access and replace parts (elements, slots) of \code{"Mefa"} objects.
}
\usage{
xtab(x)
"xtab<-"(x, value)

samp(x)
"samp<-"(x, value)

taxa(x)
"taxa<-"(x, value)
}
\arguments{
  \item{x}{
an object of S4 class \code{"Mefa"}.
}
  \item{value}{
replacement value.
}
}
\details{
The \code{[} method ensures that the \code{xtab} sparse matrix part and the
corresponding attribute tables are subsetted correctly.

Validity check is performed when replacing slots of an object.
}
\value{
An object of S4 class \code{"Mefa"}.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link{Mefa}}
}
\examples{
x <- data.frame(
    sample = paste("Sample", c(1,1,2,2,3,4), sep="."),
    species = c(paste("Species", c(1,1,1,2,3), sep="."),  "zero.pseudo"),
    count = c(1,2,10,3,4,0))
samp <- data.frame(samples=levels(x$sample), var1=1:2)
taxa <- data.frame(specnames=levels(x$species), var2=c("b","a"))
rownames(samp) <- samp$samples
rownames(taxa) <- taxa$specnames
x1 <- Xtab(count ~ sample + species, x)
x3 <- Mefa(x1, samp, taxa)

## accessing the xtab slot
xtab(x3)
## replacing the slot value
x1[3,1] <- 999
xtab(x3) <- x1
xtab(x3)

## accessing and replacing the samp slot
samp(x3)
samp(x3) <- NULL
samp(x3)
samp(x3) <- samp[1:3,]
samp(x3)

## accessing and replacing the taxa slot
taxa(x3)
taxa(x3) <- NULL
taxa(x3)
taxa(x3) <- taxa[1:3,]
taxa(x3)

## subsetting
unclass(x3[3:2, 1:2])
unclass(x3[3:2,])
unclass(x3[,1:2])
}
\keyword{ manip }
