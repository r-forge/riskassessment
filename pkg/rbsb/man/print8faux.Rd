\name{print8faux}
\alias{print8faux}
\title{
prints a faux object
}
\description{
This function prints in a interpreted way a /faux/ object.
}
\usage{
print8faux(x, ..., what=2)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 print(rbsb.fau1, what=2);
}
\arguments{
\item{x}{The faux object to be printed.}
\item{\dots}{Further arguments to be passed to the print function.}
\item{what}{the level of printing 0= only the definition, 1= the
origin and the definition 2= e}
}
\details{
Global constant \code{rbsb.mwi} is used to justify the paragraphes.
It is the specific print for /faux/ objects.
}
\value{
nothing but a print is performed
}
\keyword{error}
