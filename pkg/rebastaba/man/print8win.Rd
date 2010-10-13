\name{print8win}
\alias{print8win}
\title{
prints a /win/
}
\description{
(ba) prints a /win/ object after checking its validity
}
\usage{
print8win(x, ..., fo=1)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 print(rebastaba.win0);
 print(rebastaba.win1);
 print(rebastaba.win2);
}
\arguments{
\item{x}{win object}
\item{\dots}{Further arguments to be passed to the print function. In
fact, not used but kept for the consistence with the other print
functions.}
\item{fo}{ format to use, 1 for standard, 0 for reduced.}
}
\value{
nothing a printing is issued
}
\keyword{print}
