\name{range8pos}
\alias{range8pos}
\title{
returns the ranges (x,y) for a pos
}
\description{
(ba) This function returns the ranges (x,y) for a pos taking into
account the zoom specification of the pos and the extreme values of
its positions.
}
\usage{
range8pos(pos, padding=g4n.pgr0@padding)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 uu <- new("pos", posi=matrix(round(10*cos(1:20)), 5));
 range8pos(uu);
}
\arguments{
\item{pos}{The pos object.}
\item{padding}{The padding coefficient to apply.}
}
\value{
a list with two components \$x and \$y giving the ranges (min,max) of
the two first positions
}
\keyword{utilities}
