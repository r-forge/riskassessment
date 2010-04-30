\name{geneal4pam}
\alias{geneal4pam}
\title{
(ba) constructs the genealoty of a given node
}
\description{
This function constructs the genealoty of a given node, that is
returns the graph of all ascendants (descendants) starting from this
node.
}
\usage{
geneal4pam(pam, i, asc=TRUE)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 geneal4pam(rbsb.pam1, 2);
}
\arguments{
\item{pam}{The pam matrix describing the parentship.}
\item{i}{The considered node.}
\item{asc}{(=TRUE) must ascendants be looked for, if not the
descendants.}
}
\value{
an arc object describing the obtained tree
}
\keyword{utilities}
