\name{nv3nom}
\alias{nv3nom}
\title{
detects the existence of names into x2nom(nom)
}
\description{
Detects the existence of 'na' as a valid node names or a valid
variable names included into \code{nom} or \code{nom@nom}.
}
\usage{
nv3nom(na, nom)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 nv3nom(c("C[1]", "C[10]", "2", "C", "B", "a"), rbsb.nom3);
}
\arguments{
\item{na}{name vector to be looked for}
\item{nom}{nom object to make the correspondence.}
}
\details{
In case of identical variable and node names, the node will be
retained: \code{nv3nom("a",new("nom",x=list(a="a")))} returns 1, not
-1.
}
\value{
a named (with \code{na}) numeric vector of \code{length(na)}. Its
values are: -i when a variable name of the ith node 0 when no
correspondance; 1 when a node name with variable(s); 2 when a
node[variable] name; 3 when a node name without variable.
}
\keyword{misc}
