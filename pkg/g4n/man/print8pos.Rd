\name{print8pos}
\alias{print8pos}
\title{
prints a /pos/
}
\description{
(ba) just defining the print.pos function
}
\usage{
print8pos(x, ...)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 uu <- new("pos", posi=matrix(round(10*cos(1:20)), 5));
 print8pos(uu);
}
\arguments{
\item{x}{the pos object to be printed}
\item{\dots}{Further arguments to be passed to the print function.}
}
\value{
nothing a printing is issued
}
\keyword{print}
