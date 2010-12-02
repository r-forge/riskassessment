\name{print8assess}
\alias{print8assess}
\title{
prints an /assess/ object
}
\description{
prints, with or more emphasis, an /assess/ object.
}
\usage{
print8assess(x, ..., empha=0)
}
\examples{
 prr3k("RESET"); # needed only for R checking, to be forgotten
 print(prr.assess1);
 print(prr.assess1, empha=2);
}
\arguments{
\item{x}{assess object to be printed}
\item{\dots}{Further arguments to be passed to the \code{print}
function.}
\item{empha}{ degree of emphasis, between 0 and 10.}
}
\value{
nothing a printing is issued
}
\keyword{print}
