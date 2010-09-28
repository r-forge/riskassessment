\name{form3titre}
\alias{form3titre}
\title{
prints or prepares a title
}
\description{
prints a character string "tit" with more or less emphasis. This
function is a shortcut of \code{form3title} indeed, it just some
specialized calls to it.
}
\usage{
form3titre(tit, empha=3, indent=2+2*empha, imp=TRUE)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 form3titre("Some Title");
}
\arguments{
\item{tit}{the title to print (just one line)}
\item{empha}{ Level of emphasize.\cr 0: single line without carriage
return\cr 1: single line\cr 2: underlined\cr 3: underlined and
overlined\cr 4: 2 + 1 line before\cr 5: 3 + 1 line after\cr 6: 2 + 2
lines before and after\cr 7: ...}
\item{indent}{ Number of spaces to introduce before the title}
\item{imp}{ Printing is performed and nothing is returned. If FALSE,
the character string is returned (including possible new lines)}
}
\value{
either nothing or a character string according to imp
}
\keyword{print}
