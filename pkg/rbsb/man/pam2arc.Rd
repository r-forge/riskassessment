\name{pam2arc}
\alias{pam2arc}
\title{
(ba) from the parentship object pam returns the arcs object
}
\description{
from the parentship object pam returns the arcs object
}
\usage{
pam2arc(pam)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 print(pam2arc(rbsb.pam0));
 print(pam2arc(rbsb.pam1));
}
\arguments{
\item{pam}{The object pam.}
}
\value{
the arc object.
}
\keyword{utilities}
