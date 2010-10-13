\name{pam2arc}
\alias{pam2arc}
\title{
from the parentship object pam returns the arcs object
}
\description{
(ba) from the parentship object pam returns the arcs object
}
\usage{
pam2arc(pam)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 print(pam2arc(g4n.pam0));
 print(pam2arc(g4n.pam1));
}
\arguments{
\item{pam}{The object pam.}
}
\value{
the arc object.
}
\keyword{utilities}
