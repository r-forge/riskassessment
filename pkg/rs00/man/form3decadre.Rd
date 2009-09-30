\name{form3decadre}
\alias{form3decadre}
\title{
(f0) remove character before and after a character string
}
\description{
remove 'bef's before and 'aft's after a character string.
}
\usage{
form3decadre(chaine, bef="", aft=bef)
}
\examples{
 form3decadre('IMPORTANT', 'IM', ' ANT');
}
\arguments{
\item{chaine}{The character string to refine. Must be of length one.}
\item{bef}{ What to repeatedly remove before.}
\item{aft}{ What to repeatedly remove after.}
}
\value{
character string after removing
}
\keyword{utilities}
