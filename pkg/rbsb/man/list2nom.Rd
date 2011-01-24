\name{list2nom}
\alias{list2nom}
\title{
transforms a list into a /nom/
}
\description{
simply returns \code{nom} with its unique slot \code{@x} being equal
to \code{lili}. This function was introduced for consistency with
other objects and to prepare future evolutions
}
\usage{
list2nom(lili)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 list2nom(rbsb.nom3@x);
}
\arguments{
\item{lili}{The \code{list} object to be transformed.}
}
\details{
According to \code{rbsb.mck} the produced object is checked.
}
\value{
The resulting \code{nom}.
}
\keyword{misc}
