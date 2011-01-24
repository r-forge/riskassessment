\name{form3decadre}
\alias{form3decadre}
\title{
removes framing characters from a character string
}
\description{
removes \code{bef}s before and \code{aft}s after a character string.
}
\usage{
form3decadre(chaine, bef=rbsb.sep0, aft=bef, mxm=Inf)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 form3decadre('IMPORTANT', 'IM', ' ANT');
 form3decadre(c(' OUF ', ' FOU ', ''), ' ', '', 1);
}
\arguments{
\item{chaine}{The character string to refine. Can be a vector.}
\item{bef}{ What to repeatedly remove before.}
\item{aft}{ What to repeatedly remove after.}
\item{mxm}{ Maximum number of tags to remove.}
}
\value{
character string after removing
}
\keyword{utilities}
