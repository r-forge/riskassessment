\name{char2des}
\alias{char2des}
\title{
(00) transforms,if necessary, a single character to a 'des' object
}
\description{
when x is not a character(1) or a 'des' an error is issued
}
\usage{
char2des(x)
}
\examples{
 rs003k("RESET"); # needed only for R checking, to be forgotten
 print(char2des("toto"));
}
\arguments{
\item{x}{name for the created 'des' or an already existing 'des'
ojbect.}
}
\value{
a 'des' object (not modfyied when x was already some one.
}
\keyword{utilities}
