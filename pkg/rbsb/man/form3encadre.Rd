\name{form3encadre}
\alias{form3encadre}
\title{
surrounds a character string
}
\description{
Adds before and after some characters to a character string (the same
for all components).
}
\usage{
form3encadre(chaine, bef="(*)_", aft="", imp=FALSE, cr=imp)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 form3encadre('IMPORTANT', '<<< ', ' >>>');
}
\arguments{
\item{chaine}{The character string to frame; can be of length greater
than one.}
\item{bef}{ What to add before}
\item{aft}{ What to add after}
\item{imp}{ Printing when TRUE or returning (default)}
\item{cr}{ Must a line feed be added?}
}
\value{
character string or printing according to \code{imp}.
}
\keyword{print}
