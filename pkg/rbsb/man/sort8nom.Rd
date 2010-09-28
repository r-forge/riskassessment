\name{sort8nom}
\alias{sort8nom}
\title{
sorts a /nom/ according to various ways
}
\description{
Nodes and variables of the \code{nom} are sorted according the
alphabet, the number of variables.
}
\usage{
sort8nom(nom, by="Aa")
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 sort8nom(rbsb.nom3, "S");
 print(sort8nom(rbsb.nom7, "Aa"));
}
\arguments{
\item{nom}{The \code{nom} object to be sorted.}
\item{by}{A \code{character(1)} indicating the kind of sorting by the
letters in it. \code{a} for sorting the variables within the node in
alphabetical order. \code{A} for the nodes and/or \code{S} with
respect to their numbers of variates.}
}
\details{
Compatible criteria can be used simultaneously.
}
\value{
The resulting \code{nom}.
}
\keyword{misc}
