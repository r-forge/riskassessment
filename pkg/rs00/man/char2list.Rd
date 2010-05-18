\name{char2list}
\alias{char2list}
\title{
(00) transforms a character into a list
}
\description{
Just doing a list of single (or not) characters from a a character
vector, taking care of the names in a due way.
}
\usage{
char2list(cara, tw="", nam=NULL)
}
\examples{
 char2list(c("A B C", "D E"));
 char2list(c("A B C", "D E"), " ");
}
\arguments{
\item{cara}{ The vector of characters to transform. When is.null(nam)
and they exist, the names of the vector are reported to the list.}
\item{tw}{ Separators for the second step splitting.}
\item{nam}{ Names to be apply to the created list. The exact number
is expected, as well as non repeated names.}
}
\details{
Argument 'tw' is a possible separator. If empty only one character
each component. If not each caracter is splitted with it...
}
\value{
a list of single character with possibly named components
}
\keyword{utilities}
