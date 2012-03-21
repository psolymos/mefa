\name{kkjokes}
\alias{kkjokes}
\docType{data}
\title{
List with Knock-Knock Jokes
}
\description{
List with the ingredients of Knock-Knock Jokes.
}
\usage{data(kkjokes)}
\format{
  List with the ingredients of Knock-Knock Jokes. Elements are of \code{"KnockKnockJoke"}
  class.
}
\source{
Folklore, kid's books and Internet.
}
\examples{
data(kkjokes)
kkj <- kkjokes[[1]]
KnockKnockJoke(kkj[1], kkj[2])
}
\keyword{datasets}
