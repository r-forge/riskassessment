\name{comp3interpolation}
\alias{comp3interpolation}
\title{
extrapolation of known values
}
\description{
From a set values given at known dates (x), interpolation is done for
other dates (xx). Extrapolation is rejected.
}
\usage{
comp3interpolation(values, x, xx=1:x[max(x)])
}
\arguments{
\item{values}{values to be interpolated}
\item{x}{dates associated to the known values}
\item{xx}{dates for which values must be found by linear
interpolation.}
}
\value{
The resulting values (in the same order that xx)
}
\keyword{utilities}
