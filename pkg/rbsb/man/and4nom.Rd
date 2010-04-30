\name{and4nom}
\alias{and4nom}
\title{
(ba) adds one node to a /nom/
}
\description{
returns \code{nom} after adding it a new node.
}
\usage{
and4nom(nom, nod, var="")
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 print(rbsb.nom2);
 print(and4nom(rbsb.nom2, "D", "z"));
}
\arguments{
\item{nom}{The /nom/ to be completed.}
\item{nod}{Name for the new node (character(1)).}
\item{var}{Name(s) for the variable of the new node.}
}
\value{
The /nom/ completed
}
\keyword{misc}
