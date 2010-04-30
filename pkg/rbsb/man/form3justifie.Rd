\name{form3justifie}
\alias{form3justifie}
\title{
(00) formats a character string
}
\description{
The aim of this function is to produce aligned columns lists while
printing the rows not a the same time.
}
\usage{
form3justifie(chaine, nbc=8, format=3, tronc=TRUE, carac="")
}
\examples{
 form3justifie("vers")
 form3justifie("versification", 5)
}
\arguments{
\item{chaine}{the character string to be printed}
\item{nbc}{ Desired number of characters for the result}
\item{format}{ Indicates the type of alignment:\cr 0 no aligment (no
supplementary character added to reach nbc)\cr 1 to the left side\cr
2 centered\cr 3 to the right side}
\item{tronc}{ If true, no more than "nbc" characters are returned and
possibly the string is truncated. In that case, \$ is added for
indicating the fact.}
\item{carac}{ Character to use for enlarging the string}
}
\value{
a character string
}
\keyword{print}
