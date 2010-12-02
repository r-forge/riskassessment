\name{belong2interval}
\alias{belong2interval}
\title{
checks if a series of values belong to a series of intervals
}
\description{
computes and returns the indicator vector of the positions of values
with respect to intervals.
}
\usage{
belong2interval(x, int)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 belong2interval(1:5, 1:2);
 belong2interval(1:5, matrix(1:10, ncol=2));
}
\arguments{
\item{x}{vector of value(s) to be scrutinized.}
\item{int}{series of interval(s) to be considered. Either a
numeric(2) or a matrix with two columns.\cr Empty intervals
(\code{numeric(0)}= are not admitted.}
}
\value{
A matrix with rows associated to the \code{x} values and columns
associated to the \code{int} intervals giving \code{-2,-1,0,1,2}
according to whether \code{x} is less than, equal to the lower bound,
inside, equal to the upper bound or greater than the interval.
}
\keyword{misc}
