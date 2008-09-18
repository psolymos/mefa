\name{vhabitat}
\alias{vhabitat}
\docType{data}
\title{Land Snail Distribution Data}
\description{
Land snail distribution data and habitat features from the Villany Hills, South Hungary. 
}
\usage{
data(vhabitat)
data(vsample)
data(vtable)}
\format{
  \code{vhabitat} is a data frame with habitat features (site, site description, location, vegetation, plant cover, base rock and aspect) of 18 habitats.
  
  \code{vsample} is a data frame with 72 samples, contains numeric sample identifier, numeric site (habitat) identifier and short name of sites (habitats). Habitat identifiers refer to rows of \code{vhabitat}.
  
  \code{vtable} is a data frame with count data of 27 species (rows) and 72 samples (columns, same as rows in
    \code{vsample}).
}
\source{
Solymos, P. (2000): Comparative malacological survey of the Szarsomlyo and the Fekete Hill (Villany Hills, S Hungary). In: Uherkovich, A. (ed.): A Villanyi-hegyseg botanikai es zoologiai alapfelmerese. Dunantuli Dolgozatok (A) Termeszettudomanyi Sorozat, 10: 111-120.
\url{http://www.univet.hu/users/psolymos/personal/pdf/solymos_dtdolg_2000_villany.pdf}
}
\seealso{
  \code{\link{vsample}}, \code{\link{vtable}}
}
\examples{
data(vhabitat)
data(vsample)
data(vtable)
}
\keyword{datasets}
