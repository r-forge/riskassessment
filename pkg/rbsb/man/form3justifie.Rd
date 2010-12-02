\name{form3justifie}
\alias{form3justifie}
\title{
formats a character string
}
\description{
Formats character string(s). The main use of this function is to
produce aligned columns lists while printing the rows not at the same
time.
}
\usage{
form3justifie(chaine, nbc=8, format=3, tronc=TRUE, carac="")
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 form3justifie("vers")
 form3justifie("versification", 5)
 form3justifie(letters[1:5], format=2, carac="+");
}
\arguments{
\item{chaine}{the character string to be printed, can be a vector.}
\item{nbc}{ Desired number of characters for the result; when
\code{chain} is a vector can be a vector of the same length}
\item{format}{ Indicates the type of alignment:\cr 0 no aligment (no
supplementary character added to reach \code{nbc})\cr 1 to the left
side\cr 2 centered\cr 3 to the right side}
\item{tronc}{ If true, no more than \code{nbc} characters are
returned and possibly the string is truncated. In that case, \code{$}
is added for indicating the fact.}
\item{carac}{ Character to use for enlarging the string}
}
\value{
a character string
}
\keyword{print}
