\name{numero}
\alias{numero}
\title{
(00) returns the positions of identifiers
}
\description{
From the complete and ordered list of identifiers returns the
positions of some of them. If they don't belong to the list, an error
is issued according to \code{strict}. However they can be repeated.
}
\usage{
numero(qui, dans, strict=TRUE)
}
\examples{
 rs003k("RESET"); # needed only for R checking, to be forgotten
 numero(10:1, 1:10);
 numero(c("Z", "E", "P", "L", "I", "N"), LETTERS);
 numero(c("B", "o", "F", "!"), LETTERS, strict=FALSE);
}
\arguments{
\item{qui}{Identifiers the position of them is looked for. Must be a
character or a numeric.}
\item{dans}{The ordered list of all identifiers. Must be a character
or a numeric.}
\item{strict}{Must \code{qui} be a subset of \code{dans}.}
}
\value{
A numeric vector of the positions. It is named with the identifiers.
When the identifier does not belong to the list and \code{strict} is
FALSE, NA values are returned.
}
