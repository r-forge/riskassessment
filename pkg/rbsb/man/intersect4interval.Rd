\name{intersect4interval}
\alias{intersect4interval}
\title{
computes the interval intersection of two intervals
}
\description{
computes and returns the interval (vector of length 2 or 0) which is
the intersection of two given intervals.\cr Null intervals are
indicated by \code{rbsb.num0}.
}
\usage{
intersect4interval(int1, int2)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 intersect4interval(numeric(0), 1:2);
 intersect4interval(c(1, 10), c(-3, 5));
 intersect4interval(c(1, 10), c(10, 12));
 intersect4interval(c(1, 10), c(11, 12));
}
\arguments{
\item{int1}{The first interval (numeric(2) or numeric(0)).}
\item{int2}{The second interval (numeric(2) or numeric(0)).}
}
\value{
A numeric(2) or numeric(0) providing the intersection of the two
input intervals.
}
\keyword{misc}
