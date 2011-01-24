\name{dempirical}
\alias{dempirical}
\title{
returns the density of an /empirical/
}
\description{
This function returns the probability density of an /empirical/
object as a list with two components: \code{w} for the discrete part
and \code{y} for the continuous part associated to different values
of the random variable.
}
\usage{
dempirical(x, empi)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 dempirical(seq(0, 4, 0.5), new("empirical"));
}
\arguments{
\item{x}{Vector of values for which the density is desired.}
\item{empi}{The \code{empirical} object to consider.}
}
\details{
the proposed \code{empi} is normalized before the computation. So if
you don't what that, use by yourself the two functions
\code{continuous2density} and \code{discrete2density}.
}
\value{
A list with two components \code{$w,$y} having each the same length
as \code{x};
}
