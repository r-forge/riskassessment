\name{x2nom}
\alias{x2nom}
\title{
(ba) returns the "nom" slot of an object
}
\description{
when 'x' is a 'nom' object returns it. Else checks if \code{x@nom}
exists and returns it, if not a fatal error is issued.
}
\usage{
x2nom(x)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 identical(rbsb.nom0, x2nom(rbsb.nom0));
}
\arguments{
\item{x}{the bn/gn/dn/gn/.. or nom object}
}
\value{
a 'nom' object
}
\keyword{misc}
