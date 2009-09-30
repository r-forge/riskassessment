\name{var3standard}
\alias{var3standard}
\title{
(f0) provides nvar standard names of variables
}
\description{
numbering surrounded with 'bef' and 'aft'
}
\usage{
var3standard(nvar, bef="-", aft="-")
}
\examples{
 var3standard(5);
}
\arguments{
\item{nvar}{Number of variables names to provide. When less than 2,
'' is returned.}
\item{bef}{ To put before the number.}
\item{aft}{ To put after the number.}
}
\value{
A character of length nvar with standard names for repetitions. When
'nvar' is one, an empty character is returned.
}
\keyword{utilities}
