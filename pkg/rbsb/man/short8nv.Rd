\name{short8nv}
\alias{short8nv}
\title{
(ba) condenses in a unique way a set of nodes/variables
}
\description{
Redundant nodes are eliminated, variables are restricted to their
nodes.
}
\usage{
short8nv(nv, nom, sort="n")
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 uu <- rbsb.nom3;
 short8nv(c("B", "C[2]", "C[1]", "B"), uu); # -> c("B", "C")
 short8nv(c("A[b]", "A", "C[2]"), uu); # -> c("A", "C[3]")
}
\arguments{
\item{nv}{character giving the nodes/variables as names.}
\item{nom}{associated 'nom' structure to perform the sorting.}
\item{sort}{(='n') the way to make the sorting 'n': according to
'nom' 'a'; according to the alphabet.}
}
\value{
a shortened character comprising the same series of nodes/variables
}
\keyword{misc}
