\name{var3standard}
\alias{var3standard}
\title{
provides nvar standard names of variables
}
\description{
numbering surrounded with 'bef' and 'aft'
}
\usage{
var3standard(nvar, bef="-", aft="-")
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 var3standard(5);
}
\arguments{
\item{nvar}{Number of variables names to provide. When less than 2,
\code{""} is returned.}
\item{bef}{ To put before the number.}
\item{aft}{ To put after the number.}
}
\value{
A character of length \code{nvar} with standard names for
repetitions. When \code{nvar} is one, an empty character is returned.
}
\keyword{utilities}
