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
\item{empha}{ Level of}
 -1: single line without carriage return

 0: single line

 1: underlined

 2: underlined and overlined

 3: 2 + 1 line before

 4: 3 + 1 line after

 5: 2 + 2 lines before and after\eqn{>>}

\item{indent}{ Number of spaces to introduce before the title}
\item{imp}{ Printing is performed and nothing is returned. If FALSE,
the character string is returned (including possible new lines)}
}
\value{
either nothing or a character string according to imp
}
\keyword{print}
