\name{form3titre}
\alias{form3titre}
\title{
(00) prints or prepares a title
}
\description{
prints a character string "tit" with more or less emphasis
}
\usage{
form3titre(tit, empha=2, indent=2+2*empha, imp=TRUE)
}
\examples{
 form3titre("Some Title");
}
\arguments{
\item{tit}{the title to print}
\item{empha}{ Level of emphasize.\cr -1: single line without carriage
return\cr 0: single line\cr 1: underlined\cr 2: underlined and
overlined\cr 3: 2 + 1 line before\cr 4: 3 + 1 line after\cr 5: 2 + 2
lines before and after}
\item{indent}{ Number of spaces to introduce before the title}
\item{imp}{ Printing is performed and nothing is returned. If FALSE,
the character string is returned (including possible new lines)}
}
\value{
either nothing or a character string according to imp
}
\keyword{print}
