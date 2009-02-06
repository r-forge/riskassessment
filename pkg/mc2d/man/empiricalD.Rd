\name{empiricalD}
\alias{empiricalD}
\alias{dempiricalD}
\alias{pempiricalD}
\alias{qempiricalD}
\alias{rempiricalD}
\title{
The Discrete Empirical Distribution
}
\description{
Density, distribution function and random generation for a discrete
empirical distribution. This function is vectorized to accept
different sets of values or prob.
}
\usage{
dempiricalD(x, values, prob=NULL, log=FALSE)
pempiricalD(q, values, prob=NULL, lower.tail=TRUE, log.p=FALSE)
qempiricalD(p, values, prob=NULL, lower.tail=TRUE, log.p=FALSE)
rempiricalD(n, values, prob=NULL)
}
\examples{
dempiricalD(1:6, 2:6, prob=c(10, 10, 70, 0, 10))
pempiricalD(1:6, 2:6, prob=c(10, 10, 70, 0, 10))
qempiricalD(seq(0, 1, 0.1), 2:6, prob=c(10, 10, 70, 0, 10))
table(rempiricalD(10000, 2:6, prob=c(10, 10, 70, 0, 10)))

## varying values
(values <- matrix(1:10, ncol=5))
## the first x apply to the first row : p = 0.2
## the second x to the second one: p = 0
dempiricalD(c(1, 1), values)



}
\arguments{
\item{x, q}{Vector of quantiles.}
\item{p}{Vector of probabilities.}
\item{n}{Number of random values. If length(n) \eqn{>} 1, the length
is taken to be the number required.}
\item{values}{Vector or matrix of numerical values. See details.}
\item{prob}{Optionnal vector or matrix of count or probabilities. See
details.}
\item{log, log.p}{logical; if TRUE, probabilities p are given as
log(p).}
\item{lower.tail}{logical; if TRUE (default), probabilities are P[X
\eqn{<}= x] otherwise, P[X \eqn{>} x].}
}
\details{
If prob is missing, the discrete distribution is obtained directly
from the vector of values, otherwise prob is used to weight the
values. prob is normalized before use. Thus, prob may be the count of
each values. prob values should be non negative and their sum should
not be 0.

 values and/or prob may vary: in that case, values and/or prob should
be sent as matrixes, the first row being used for the first element
of x, q, p or the first random value, the second row for the second
element of x, q, p or random value, ... Recycling is permitted if the
number of rows of prob and values are equal or if the number of rows
of prob and/or values are one.

 rempiricalD(n, values, prob) with values and prob as vectors is
equivalent to \code{sample(x=values, size=n, replace=TRUE,
prob=prob)}.
}
\value{
dempiricalD gives the density, pempiricalD gives the distribution
function, qempiricalD gives the quantile function and rempiricalD
generates random deviates.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{sample}}. \code{\link{empiricalC}}.
}
\note{
In the future, the fonctions should be written for non numerical
values.
}
\keyword{distribution}
