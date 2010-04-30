\name{form3decadre}
\alias{form3decadre}
\title{
(00) remove character before and after a character string
}
\description{
remove 'bef's before and 'aft's after a character string.
}
\usage{
form3decadre(chaine, bef="", aft=bef, mxm=Inf)
}
\examples{
 rs003k("RESET"); # needed only for R checking, to be forgotten
 form3decadre('IMPORTANT', 'IM', ' ANT');
 form3decadre(' OUF ', ' ', '', 1);
}
\arguments{
\item{chaine}{The character string to refine. Must be of length one.}
\item{bef}{ What to repeatedly remove before.}
\item{aft}{ What to repeatedly remove after.}
\item{mxm}{ Maximum number of characters to be removed.}
}
\value{
character string after removing
}
\keyword{utilities}
