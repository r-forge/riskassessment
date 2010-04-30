\name{list2char}
\alias{list2char}
\title{
(00) transforms a list into a character vector
}
\description{
The resulting vector will have as many components as list. Each one
being the concatenation of its own components interpreted
\code{as.character} with \code{tw} as seperator.
}
\usage{
list2char(liste, tw="", nam=NULL)
}
\examples{
 list2char(char2list(c("A B C", "D E")));
 list2char(char2list(c("A B C", "D E"), ""), "");
 char2list(list2char(char2list(c("A B C", "D E"))));
 char2list(list2char(char2list(c("A B C", "D E"), ""), ""), "");
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
