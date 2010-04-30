\name{rmnd4nom}
\alias{rmnd4nom}
\title{
(ba) removes one node to a /nom/
}
\description{
removes one (and only one) node to a \code{/nom/}.
}
\usage{
rmnd4nom(nom, nod)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 print(rmnd4nom(rbsb.nom2, "A"));
}
\arguments{
\item{nom}{The /nom/ to be restricted.}
\item{nod}{Name of the name to be removed (character(1)).}
}
\value{
The reducede /nom/
}
\keyword{misc}
