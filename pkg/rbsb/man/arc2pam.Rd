\name{arc2pam}
\alias{arc2pam}
\title{
(ba) constructs the object pam from the object arc
}
\description{
exploring the matrix arc@fle, returns a binary matrix with rows
associated to each node giving their children. 0 means that this
column is not a children as a pam object.
}
\usage{
arc2pam(arc)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 print(rbsb.arc0);
 print(arc2pam(rbsb.arc0));
}
\arguments{
\item{arc}{The arc object}
}
\value{
the pam object
}
\keyword{utilities}
