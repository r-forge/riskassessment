\name{qempirical}
\alias{qempirical}
\title{
returns the quantiles of an /empirical/
}
\description{
This function returns the quantiles of an /empirical/ object for
different probabilities.
}
\usage{
qempirical(p, empi)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 pempirical(seq(0, 1, 0.2), new("empirical"));
}
\arguments{
\item{p}{Vector of probabilities for which the quantile are wanted.}
\item{empi}{The \code{empirical} object to consider.}
}
\value{
A vector with same length as \code{p} containing the quantiles.
}
