\name{print8ion}
\alias{print8ion}
\title{
prints a 'ion' object
}
\description{
(ba) print associated to a \code{/ion/} object.
}
\usage{
print8ion(x, how="n")
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 print(rbsb.ion0);
}
\arguments{
\item{x}{the 'ion' object to be printed.}
\item{how}{ the way to make the printing: 'n' for node names 'v' for
variable names 'i' for node numbers 'j' for nested variable numbers
'k' for variable numbers 'a' for everything.}
}
\value{
Nothing but a printing is issued
}
\keyword{print}
