\name{print8faux}
\alias{print8faux}
\title{
(f0) prints a faux object
}
\description{
This function prints in a interpreted way a /faux/ object.
}
\usage{
print8faux(x, ..., print=1)
}
\examples{
 uu <- new("faux", orig="ici", defi="Fatal Error", comm=c("In fact it was a joke", "No error occurred!", "You must be relieved"));
 print(uu, print=2);
}
\arguments{
\item{x}{The faux object to be printed.}
\item{\dots}{Further arguments to be passed to the print function.}
\item{print}{(=1) the level of printing 0= only the definition, 1=
the origin and the definition 2= }
}
\details{
Global constant rbsb3k("mwi") is used to justify the paragraphes. It
is the generic print for /faux/ objects.
}
\value{
nothing but a print is performed
}
\keyword{error}
