\name{print8nod}
\alias{print8nod}
\title{
prints a /nod/ object
}
\description{
prints, with or more emphasis, a /nod/ object.
}
\usage{
print8nod(x, ..., empha=0)
}
\examples{
 prr3k("RESET"); # needed only for R checking, to be forgotten
 print(prr.nod1);
 print(prr.nod1, empha=2);
}
\arguments{
\item{x}{nod object to be printed}
\item{\dots}{Further arguments to be passed to the \code{print}
function.}
\item{empha}{ degree of emphasis, between 0 and 10.}
}
\value{
nothing a printing is issued
}
\keyword{print}
