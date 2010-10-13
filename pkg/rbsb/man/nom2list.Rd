\name{nom2list}
\alias{nom2list}
\title{
transforms a /nom/ into a list
}
\description{
simply returns \code{nom@x} which is a list. This function was
introduced for consistency with other objects and to prepare future
evolutions
}
\usage{
nom2list(nom)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 nom2list(rbsb.nom3);
}
\arguments{
\item{nom}{The \code{nom} object to be transformed.}
}
\value{
The resulting \code{list}.
}
\keyword{misc}
