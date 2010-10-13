\name{nom2char}
\alias{nom2char}
\title{
transforms a /nom/ into a character
}
\description{
returns a character of the variables or node from a \code{/nom/}.
}
\usage{
nom2char(nom, of="v", sort=FALSE)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 nom2char(rbsb.nom3);
 nom2char(rbsb.nom3, "n");
}
\arguments{
\item{nom}{The \code{nom} object to be transformed.}
\item{of}{'v' to return the set of variables; 'n' to return the set
of involved nodes.}
\item{sort}{Must the nodes and names be sorted?}
}
\details{
Notice that \code{char2nom(nom)} is equivalent to
\code{nv2ion(0,nom,"v")@nvn}.
}
\value{
a \code{character}.
}
\keyword{misc}
