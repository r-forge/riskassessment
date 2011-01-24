\name{form3line}
\alias{form3line}
\title{
prints a separator line from a given pattern
}
\description{
Prints a separator line from a given pattern. The line can be
composite with the width argument. The pattern can comprise more than
one character. General indentation is possible (\code{gind}). Even
lines can have a different pattern(\code{pat2}).
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
\item{gind}{ String to introduce as indentation.}
\item{pat2}{ Pattern to use for the even lines. If NULL, the pattern
is common to all lines.}
\item{imp}{ Printing when TRUE or returning (FALSE)}
}
\details{
contrary to \code{form3repeat} \\n are introduced at the end.
}
\value{
character string or printing according to \code{imp}
}
\keyword{print}
