\name{form3encadre}
\alias{form3encadre}
\title{
surrounds a character string
}
\description{
Adds \_bef\_ore and \_aft\_er some characters to a character string
(the same for all components when it is a vector).
}
\usage{
form3encadre(chaine, bef="(*)_", aft="", imp=FALSE, cr=imp)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 form3encadre('IMPORTANT', '<<< ', ' >>>');
 form3encadre('IMPORTANT', '<<< ', ' >>>', imp=TRUE);
}
\arguments{
\item{chaine}{The character string to frame; can be a vector.}
\item{bef}{ What to add before}
\item{aft}{ What to add after}
\item{imp}{ Printing when TRUE or returning}
\item{cr}{ Must a line feed be added?}
}
\value{
character string or printing according to \code{imp}.
}
\keyword{print}
