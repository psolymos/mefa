\name{Xtab}
\alias{Xtab}
\alias{sparseMatrixList-class}
\title{
Sparse Cross Tabulation
}
\description{
Create a contingency table from cross-classifying factors, usually contained in a data frame, using a formula interface. 
}
\usage{
Xtab(formula = ~., data = parent.frame(), rdrop, cdrop,
subset, na.action, exclude = c(NA, NaN), drop.unused.levels = FALSE)
}
\arguments{
  \item{formula}{
a \code{\link{formula}} object with the cross-classifying variables (separated by +) on the right hand side (or an object which can be coerced to a formula). Interactions are not allowed. On the left hand side, one may optionally give a vector or a matrix of counts; in the latter case, the columns are interpreted as corresponding to the levels of a variable. This is useful if the data have already been tabulated, see the examples below.
}
  \item{data}{
an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula formula. By default the variables are taken from environment(formula).
}
  \item{rdrop, cdrop}{
logical (should zero marginal rows/columns be removed after cross tabulation), character or numeric (what rows/columns should be removed).
}
  \item{subset}{
an optional vector specifying a subset of observations to be used.
}
  \item{na.action}{
a function which indicates what should happen when the data contain NAs.
}
  \item{exclude}{
a vector of values to be excluded when forming the set of levels of the classifying factors.
}
  \item{drop.unused.levels}{
a logical indicating whether to drop unused levels in the classifying factors. If this is FALSE and there are unused levels, the table will contain zero marginals, and a subsequent chi-squared test for independence of the factors will not work.
}
}
\details{
The function creates two- or three-way cross tabulation. Only works for two or three factors. 

If a left hand side is given in formula, its entries are simply summed over the cells corresponding to the right hand side; this also works if the left hand side does not give counts. 
}
\value{
A sparse numeric matrix inheriting from \code{\link{sparseMatrix}}, specifically an object of S4 class \code{dgCMatrix}. 

For three factors, a list of sparse matrices.
}
\author{
This function is a slight modification of the \code{\link[Matrix]{xtabs}} function in the \pkg{Matrix} package.

Modified by Peter Solymos <solymos@ualberta.ca>
}
\seealso{
See also \code{\link[stats]{xtabs}} in \pkg{stats} and \code{\link[Matrix]{xtabs}} in \pkg{Matrix} package.

\code{"mefa"} S3 class: \code{\link[mefa]{mefa}} in \pkg{mefa} package.
}
\examples{
x <- data.frame(
    sample = paste("Sample", c(1,1,2,2,3,4), sep="."),
    species = c(paste("Species", c(1,1,1,2,3), sep="."),  "zero.pseudo"),
    count = c(1,2,10,3,4,0))
x
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

## 2 and 3 way crosstabs
xx <- data.frame(
    sample = paste("Sample", c(1,1,2,2,3,4), sep="."),
    species = c(paste("Species", c(1,1,1,2,3), sep="."),  "zero.pseudo"),
    count = c(1,2,10,3,4,0),
    segment = letters[c(6,13,6,13,6,6)])
xx
Xtab(count ~ sample + species, xx)
Xtab(count ~ sample + species + segment, xx)
}
\keyword{ manip }
