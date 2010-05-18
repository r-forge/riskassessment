\name{form3line}
\alias{form3line}
\title{
(00) print a separator line from a given pattern
}
\description{
The line can be composite with the width arguement. The pattern can
comprise more than one character. General indentation is possible
(gind). Even lines can have a different pattern(pat2).
}
\usage{
form3line(len=50, pat="-_-", wid=3, gind="", pat2="_-_", imp=TRUE)
}
\examples{
 form3line();
}
\arguments{
\item{len}{ Line length (without the general indentation.}
\item{pat}{ Pattern to use.}
\item{wid}{ Number of elementary lines.}
\item{gind}{ Pattern to use for the general indentation.}
\item{pat2}{ Pattern to use for the even lines. If NULL, the pattern
is common to all lines.}
\item{imp}{ Printing when TRUE or returning (FALSE)}
}
\details{
contrary to form3repete 'backslash'n are introduced including at the
end.
}
\value{
character string or printing according to imp
}
\keyword{print}
