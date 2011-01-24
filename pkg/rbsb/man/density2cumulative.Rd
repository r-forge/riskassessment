\name{density2cumulative}
\alias{density2cumulative}
\title{
returns the cumulative of an /empirical/
}
\description{
This function returns the cumulative probability distribution
function of an /empirical/ object. That is returns the coordinates of
points defining this function. Be aware that the calculus is made
numerically, which is not the case for \code{pempirical}, so better
use it.
}
\usage{
density2cumulative(empi, N=1000)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 density2cumulative(new("empirical"));
}
\arguments{
\item{empi}{The \code{empirical} object to consider.}
\item{N}{The number of steps for the curve all around the range of
the random variable.}
}
\details{
\code{N} must be understood as a simple way to give the level of
precision the user wants to see apply for the returned curve.
}
\value{
A matrix with two columns \code{x,y}.
}
