\name{li2cha}
\alias{li2cha}
\title{
transforms a list into a character vector
}
\description{
The resulting vector will have as many components as list. Each one
being the concatenation of its own components interpreted
\code{as.character} with \code{tw} as seperator.
}
\usage{
li2cha(liste, tw="", nam=NULL)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 li2cha(cha2li(c("A B C", "D E")));
 li2cha(cha2li(c("A B C", "D E"), ""), "");
 cha2li(li2cha(cha2li(c("A B C", "D E"))));
 cha2li(li2cha(cha2li(c("A B C", "D E"), ""), ""), "");
}
\arguments{
\item{liste}{ The list to }
\item{nam}{ When \code{is.null(nam)} and they exist, the names of the
list are reported to the vector.}
\item{tw}{ Separator for each component.}
}
\value{
a character vector with possibly named components
}
\keyword{utilities}
