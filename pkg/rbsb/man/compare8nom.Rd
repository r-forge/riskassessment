\name{compare8nom}
\alias{compare8nom}
\title{
compares two /nom/s
}
\description{
compares two /nom/s either at the node or at the variable level.
}
\usage{
compare8nom(noma, nomb, level="v")
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 compare8nom(rbsb.nom2, rbsb.nom3);
 compare8nom(rbsb.nom2, rbsb.nom3, "n");
 compare8nom(rbsb.nom5, rbsb.nom3);
}
\arguments{
\item{noma}{The first \code{nom} to be compared.}
\item{nomb}{The second \code{nom} to be compared.}
\item{level}{'v' or 'n' to indicate if the comparaison must be made
at the 'variable' or 'node' level.}
}
\value{
A list of three /nom/ named \code{$a_b}, \code{$b_a} and \code{$a.b}
giving the respectively the set differences and the intersection of
the choosen items.
}
\keyword{misc}
