\name{pempirical}
\alias{pempirical}
\title{
returns the distribution function of an /empirical/
}
\description{
This function returns the cumulative distribution function of an
/empirical/ object for different values of the random variable.
}
\usage{
pempirical(x, empi)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 pempirical(seq(0, 4, 0.5), new("empirical"));
}
\arguments{
\item{x}{Vector of values for which the distribution is wanted.}
\item{empi}{The \code{empirical} object to consider.}
}
\value{
A vector with same length as \code{x};
}
