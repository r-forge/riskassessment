\name{betagen}
\alias{betagen}
\alias{dbetagen}
\alias{pbetagen}
\alias{qbetagen}
\alias{rbetagen}
\title{
The Generalised Beta Distribution
}
\description{
Density, distribution function, quantile function and random
generation for the Beta distribution defined on the \code{[min, max]}
domain with parameters \code{shape1} and \code{shape2} ( and optional
non-centrality parameter \code{ncp}).
}
\usage{
dbetagen(x, shape1, shape2, min=0, max=1, ncp=0, log=FALSE)
pbetagen(q, shape1, shape2, min=0, max=1, ncp=0, lower.tail=TRUE,
	  log.p=FALSE)
qbetagen(p, shape1, shape2, min=0, max=1, ncp=0, lower.tail=TRUE,
	  log.p=FALSE)
rbetagen(n, shape1, shape2, min=0, max=1, ncp=0)
}
\examples{
curve(dbetagen(x, shape1=3, shape2=5, min=1, max=6), from = 0, to = 7)
curve(dbetagen(x, shape1=1, shape2=1, min=2, max=5), from = 0, to = 7, lty=2, add=TRUE)
curve(dbetagen(x, shape1=.5, shape2=.5, min=0, max=7), from = 0, to = 7, lty=3, add=TRUE)



}
\arguments{
\item{x,q}{Vector of quantiles.}
\item{p}{Vector of probabilities.}
\item{n}{Number of observations. If \code{length(n) > 1}, the length
is taken to be the number required.}
\item{shape1, shape2}{Positive parameters of the Beta distribution.}
\item{min}{Vector of minima.}
\item{max}{Vector of maxima.}
\item{ncp}{Non-centrality parameter of the Beta distribution.}
\item{log, log.p}{Logical; if \code{TRUE}, probabilities \code{p} are
given as \code{log(p)}.}
\item{lower.tail}{Logical; if \code{TRUE} (default), probabilities
are \code{P[X <= x]}, otherwise, \code{P[X > x]}.}
}
\details{
\deqn{x \sim betagen(shape1, shape2, min, max, ncp)}{x ~
betagen(shape1, shape2, min, max, ncp)} if
\deqn{\frac{x-min}{max-min}\sim
beta(shape1,shape2,ncp)}{(x-min)/(max-min)~beta(shape1,shape2,ncp)}
These functions use \code{\link{Beta}} distribution functions after
correct parametrisation.
}
\value{
\code{dbetagen} gives the density, \code{pbetagen} gives the
distribution function, \code{qbetagen} gives the quantile function,
and \code{rbetagen} generates random deviates.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{Beta}}
}
\keyword{distribution}
