\name{numero}
\alias{numero}
\title{
returns the positions of identifiers
}
\description{
Returns the positions of identifiers. From a reference set of
identifiers returns the positions of some of them. If they don't
belong to the list, an error is issued according to \code{strict}. If
there are repetitions an error is issued or not according to
\code{repe}.
}
\usage{
numero(qui, dans, strict=TRUE, repe=TRUE)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 numero(10:1, 1:10);
 numero(c("Z", "E", "P", "L", "I", "N"), LETTERS);
 numero(c("B", "o", "F", "!"), LETTERS, strict=FALSE);
 numero(c("B", "E", "B", "E"), LETTERS);
}
\arguments{
\item{qui}{Identifiers the position of them is looked for. Must be a
character or a numeric.}
\item{dans}{The ordered vector of all identifiers. Must be a
character or a numeric.}
\item{strict}{Must \code{qui} be a subset of \code{dans}.}
\item{repe}{Are repeatitions allowed?}
}
\value{
A numeric vector of the positions. It is named with the identifiers.
When the identifier does not belong to the list and \code{strict} is
FALSE, NA values are returned.
}
