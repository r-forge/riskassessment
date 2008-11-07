\name{triangular}
\alias{triangular}
\alias{dtriang}
\alias{ptriang}
\alias{qtriang}
\alias{rtriang}
\title{
The Triangular Distribution
}
\description{
Density, distribution function, quantile function and random
generation for the triangular distribution with minimum equal to
\code{min}, mode equal \code{mode} and maximum equal to \code{max}.
}
\usage{
dtriang(x, min=-1, mode=0, max=1, log=FALSE)
ptriang(q, min=-1, mode=0, max=1, lower.tail=TRUE, log.p=FALSE)
qtriang(p, min=-1, mode=0, max=1, lower.tail=TRUE, log.p=FALSE)
rtriang(n, min=-1, mode=0, max=1)
}
\examples{
curve(dtriang(x, min=3, mode=5, max=10), from = 2, to = 11)



}
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations. If length(n) \eqn{>} 1, the length
is taken to be the number required.}
\item{min}{vector of minima.}
\item{mode}{vector of modes.}
\item{max}{vector of maxima.}
\item{log, log.p}{logical; if \code{TRUE}, probabilities \code{p} are
given as \code{log(p)}.}
\item{lower.tail}{logical; if \code{TRUE} (default), probabilities
are \code{P[X <= x]}, otherwise, \code{P[X > x]}.}
}
\value{
\code{dtriang} gives the density, \code{ptriang} gives the
distribution function, \code{qtriang} gives the quantile function,
and \code{rtriang} generates random deviates.
}
\author{
Regis Pouillot
}
\keyword{distribution}
