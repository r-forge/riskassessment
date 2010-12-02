\name{print8chain}
\alias{print8chain}
\title{
prints a /chain/ object
}
\description{
prints, with or more emphasis, a /chain/ object.
}
\usage{
print8chain(x, ..., empha=0)
}
\examples{
 prr3k("RESET"); # needed only for R checking, to be forgotten
 print(prr.chain1);
 print(prr.chain1, empha=2);
}
\arguments{
\item{x}{chain object to be printed}
\item{\dots}{Further arguments to be passed to the \code{print}
function.}
\item{empha}{ degree of emphasis, 0 for one line printing.}
}
\value{
nothing a printing is issued
}
\keyword{print}
