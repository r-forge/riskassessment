\name{nom2nom}
\alias{nom2nom}
\title{
transform a /nom/.
}
\description{
Reduces a /nom/ to its node.
}
\usage{
nom2nom(nom, what="n")
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 print(nom2nom(rbsb.nom7));
}
\arguments{
\item{nom}{The \code{nom} object to be transformed.}
\item{what}{\code{character(1)} indicating the kind of
transformation. \code{n} for removing all the the possible variables,
leaving only the nodes.}
}
\details{
For the moment, this is the only possiblility; further on more ideas
can occur.
}
\value{
The resulting \code{nom}.
}
\keyword{misc}
