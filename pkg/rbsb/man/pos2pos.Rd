\name{pos2pos}
\alias{pos2pos}
\title{
(ba) returns the equivalent pos object with standard view
}
\description{
This function returns the equivalent pos object with standard view
(i.e. view=c(0,0)). The central point of zoom is modified
accordingly.
}
\usage{
pos2pos(pos)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 print(pos2pos(rbsb.pos));
}
\arguments{
\item{pos}{the pos object to be modified}
}
\value{
The transformed pos object
}
\keyword{utilities}
