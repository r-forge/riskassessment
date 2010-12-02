\name{print8distri}
\alias{print8distri}
\title{
prints a /distri/ object
}
\description{
prints, with or more emphasis, a /distri/ object.
}
\usage{
print8distri(x, ..., empha=0)
}
\examples{
 prr3k("RESET"); # needed only for R checking, to be forgotten
 print(prr.distri1);
 print(prr.distri1, empha=2);
}
\arguments{
\item{x}{distri object to be printed}
\item{\dots}{Further arguments to be passed to the \code{print}
function.}
\item{empha}{ degree of emphasis, between 0 and 10.}
}
\value{
nothing a printing is issued
}
\keyword{print}
