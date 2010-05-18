\name{print8faux}
\alias{print8faux}
\title{
(00) prints a faux object
}
\description{
This function prints in a interpreted way a /faux/ object.
}
\usage{
print8faux(x, ..., print=2)
}
\examples{
 rs003k("RESET"); # needed only for R checking, to be forgotten
 print(rbsb.fau1, print=2);
}
\arguments{
\item{x}{The faux object to be printed.}
\item{\dots}{Further arguments to be passed to the print function.}
\item{print}{(=1) the level of printing 0= only the definition, 1=
the origin and the definition 2= }
}
\details{
Global constant rbsb.mwi is used to justify the paragraphes. It is
the generic print for /faux/ objects.
}
\value{
nothing but a print is performed
}
\keyword{error}
