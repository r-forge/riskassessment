\name{nv3nom}
\alias{nv3nom}
\title{
(ba) detects the existence of names into nom
}
\description{
Detects the existence of 'na' as a valid node names or a valid
variable names included into nom
}
\usage{
nv3nom(na, nom)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 nv3nom(c("C[1]", "C[10]", "C", "B"), rbsb.nom2);
}
\arguments{
\item{na}{name vector to be looked for}
\item{nom}{nom object to make the correspondence.}
}
\value{
a numeric: 0 (doesn't exist), 1 (only as node name), 2 (only as
variable name) and 3 as variable and node name.
}
\keyword{misc}
