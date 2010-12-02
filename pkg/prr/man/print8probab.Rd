\name{print8probab}
\alias{print8probab}
\title{
prints a /probab/ object
}
\description{
prints, with or more emphasis, a /probab/ object.
}
\usage{
print8probab(x, ..., empha=0)
}
\examples{
 prr3k("RESET"); # needed only for R checking, to be forgotten
 print(prr.probab1);
 print(prr.probab1, empha=2);
}
\arguments{
\item{x}{probab object to be printed}
\item{\dots}{Further arguments to be passed to the \code{print}
function.}
\item{empha}{ degree of emphasis, between 0 and 10.}
}
\value{
nothing a printing is issued
}
\keyword{print}
