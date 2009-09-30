\name{form3encadre}
\alias{form3encadre}
\title{
(f0) surrounds a character string
}
\description{
Adds before and after some characters to a character string
}
\usage{
form3encadre(chaine, bef="(*)_", aft="", imp=FALSE, cr=imp)
}
\examples{
 form3encadre('IMPORTANT', '<<< ', ' >>>');
}
\arguments{
\item{chaine}{The character string to frame.}
\item{bef}{ What to add before}
\item{aft}{ What to add after}
\item{imp}{ Printing when TRUE or returning (default)}
\item{cr}{ Must a line feed be added?}
}
\value{
character string or printing according to imp
}
\keyword{print}
