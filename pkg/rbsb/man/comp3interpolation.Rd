\name{comp3interpolation}
\alias{comp3interpolation}
\title{
interpolation of known values
}
\description{
From a set of values given at known x, interpolation is done for
other values (xx). Extrapolation is rejected.
}
\usage{
comp3interpolation(values, x, xx=seq(min(x), max(x), length=100))
}
\arguments{
\item{values}{values to be interpolated (means y values)}
\item{x}{abscissae associated to the known values}
\item{xx}{dates for which values must be found by linear
interpolation.}
}
\details{
\code{x} values can be given in any order.
}
\value{
The resulting values (in the same order that xx)
}
\keyword{utilities}
