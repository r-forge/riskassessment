\name{comp3dtriang}
\alias{comp3dtriang}
\title{
returns the discrete density of a triangular distribution
}
\description{
returns the discrete density of a triangular distribution. More
precisely returns a list with \code{$xval} indicating the discretized
values and \code{$prop} the associated probabilities. When
\code{ddel} and \code{ste} are integer, the \code{$xval} are also
integer.
}
\usage{
comp3dtriang(ddel, ste=1, pos=TRUE)
}
\arguments{
\item{ddel}{(\code{numeric(3)}) Definition of the distribution
(start, mode and end abscissae).}
\item{ste}{(\code{numeric(1)}) Definition of the progression between
two values.}
\item{pos}{(\code{logical(1)}) When TRUE the interval is first
expanded of \code{ste} each side to give non null \code{$xval}.}
}
\details{
The function \code{seq} is used to compute \code{$xval}.
}
\value{
A list with two components. \code{$xval} gives the values;
\code{$prop} gives the proportion to apply to each of them
(\code{sum($prop)=1}.
}
\keyword{misc}
