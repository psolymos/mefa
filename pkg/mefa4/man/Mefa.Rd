\name{Mefa}
\alias{Mefa}
\alias{Mefa-class}
\alias{MefaDataFrame}
\alias{MefaMatrix}
\alias{mefa}
\alias{stcs}
\alias{MefaDataFrame-class}
\alias{MefaMatrix-class}
\alias{mefa-class}
\alias{stcs-class}
\alias{dim,Mefa-method}
\alias{dimnames,Mefa-method}
\alias{dimnames<-,Mefa,list-method}
\alias{t,Mefa-method}
\alias{show,Mefa-method}
\title{
'Mefa' Class
}
\description{
Creating an object of class \code{"Mefa"}.
}
\usage{
Mefa(xtab, samp, taxa, join = c("left", "inner"), drop = FALSE)
}
\arguments{
  \item{xtab}{
a matrix or a sparse matrix.
}
  \item{samp}{
a data frame or \code{NULL}.
}
  \item{taxa}{
a data frame or \code{NULL}.
}
  \item{join}{
character, \code{"left"} (default) or \code{"inner"}.
}
  \item{drop}{
logical, if unused levels in the data frames should be dropped.
}
}
\details{
\code{samp} and \code{taxa} tables are matched with corresponding dimnames in \code{xtab}:
rownames with \code{samp}, colnames with \code{taxa}.
If \code{join = "left"}, all rows and columns in \code{xtab} are retained,
while missing items in the corresponding attribute tables are filled up with \code{NA}s.
If \code{join = "inner"}, only the intersection of corresponding names are retained.

The \code{xtab} slot is a sparse matrix (\code{dgCMatrix}). The input
should be in class \code{MefaMatrix} that is a class union of \code{matrix} and \code{sparseMatrix} classes.

The \code{samp} and \code{taxa} slots take data frame or \code{NULL}, which two form
the \code{MefaDataFrame} class union.

The virtual classes \code{mefa} and \code{stcs} are defined for seamless coercion between
S3 and S4 classes.
}
\value{
An S4 object of class \code{"Mefa"} with 4 slots: \code{xtab}, \code{samp}, \code{taxa}, \code{join}.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\note{
If \code{xtab} has no dimnames, matching it up with the attribute tables can be problematic.
}
\seealso{
Creating crosstabulations: \code{\link{Xtab}}, \code{\link[stats]{xtabs}} in package \pkg{stats}, \code{\link[stats]{xtabs}} in package \pkg{stats}

\code{"mefa"} S3 class: \code{\link[mefa]{mefa}} in \pkg{mefa} package.

Accessing and replacing slots: \code{\link[mefa4]{xtab}}, \code{\link[mefa4]{samp}}, \code{\link[mefa4]{taxa}}.
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

## Xtab class, counts by repetitions in RHS
(x0 <- Xtab(~ sample + species, x))

## counts by LHS and repetitions in RHS
(x1 <- Xtab(count ~ sample + species, x))

## drop all empty rows
(x2 <- Xtab(count ~ sample + species, x, cdrop=FALSE,rdrop=TRUE))

## drop all empty columns
Xtab(count ~ sample + species, x, cdrop=TRUE,rdrop=FALSE)

## drop specific columns by placeholder
Xtab(count ~ sample + species, x, cdrop="zero.pseudo")

## Mefa class, standard
(x3 <- Mefa(x1, samp, taxa))
unclass(x3)
x3@xtab
x3@samp
x3@taxa
x3@join

## effects of left join, NULL taxa slot, xtab is (not sparse) matrix
(x4 <- Mefa(as.matrix(x1), samp[1:2,]))
unclass(x4)

## effects of inner join (intersect)
(x5 <- Mefa(x2, samp, taxa, join="inner"))
unclass(x5)
unclass(Mefa(x1, samp[1:2,], join="inner"))

## xtab only Mefa
(x6 <- Mefa(x1))

## creating new Mefa object without Mefa()
new("Mefa", xtab=x1, samp=samp, taxa=taxa,join="left")

## dim and dimnames
dim(x5)
dimnames(x5)
dn <- list(paste("S", 1:3, sep=""), paste("SPP", 1:4, sep=""))
dimnames(x5) <- dn
unclass(x5)
dimnames(x5)[[1]] <- paste("S", 1:3, sep="_")
unclass(x5)
dimnames(x5)[[2]] <- paste("SPP", 1:4, sep="_")
unclass(x5)

## transpose
x5
t(x5)
unclass(x5)
unclass(t(x5))

## 0 and 1 row/col Mefa object
x3[c(FALSE,FALSE,FALSE,FALSE),c(FALSE,FALSE,FALSE,FALSE)]
x3[c(TRUE,FALSE,FALSE,FALSE),c(FALSE,FALSE,FALSE,FALSE)]
x3[c(FALSE,FALSE,FALSE,FALSE),c(TRUE,FALSE,FALSE,FALSE)]
x3[c(TRUE,FALSE,FALSE,FALSE),c(TRUE,FALSE,FALSE,FALSE)]
}
\keyword{ manip }
