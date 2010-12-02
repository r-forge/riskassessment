\name{arc2pam}
\alias{arc2pam}
\title{
constructs the object pam from the object arc
}
\description{
(ba) exploring the matrix arc@fle, returns a binary matrix with rows
associated to each node giving their children. 0 means that this
column is not a children as a pam object.
}
\usage{
arc2pam(arc)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 print(g4n.arc0);
 print(arc2pam(g4n.arc0));
}
\arguments{
\item{arc}{The arc object}
}
\value{
the pam object
}
\keyword{utilities}
