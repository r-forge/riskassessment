\name{bernoulli}
\alias{bernoulli}
\alias{dbern}
\alias{pbern}
\alias{qbern}
\alias{rbern}
\title{
The Bernoulli Distribution
}
\description{
Density, distribution function, quantile function and random
generation for the Bernoulli distribution with probability equals to
\code{prob}.
}
\usage{
dbern(x, prob=.5, log=FALSE)
pbern(q, prob=.5, lower.tail=TRUE, log.p=FALSE)
qbern(p, prob=.5, lower.tail=TRUE, log.p=FALSE)
rbern(n, prob=.5)
}
\examples{
rbern(n=10, prob=.5)
rbern(n=3, prob=c(0, .5, 1))



}
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations. If \code{length(n) > 1}, the length
is taken to be the number required.}
\item{prob}{vector of probabilities of success of each trial.}
\item{log, log.p}{logical; if \code{TRUE}, probabilities \code{p} are
given as \code{log(p)}.}
\item{lower.tail}{logical; if \code{TRUE} (default), probabilities
are \code{P[X <= x]}, otherwise, \code{P[X > x]}.}
}
\details{
These fonctions use the corresponding functions from the
\code{\link{binomial}} distribution with argument \code{size = 1}.
Thus, 1 is for success, 0 is for failure.
}
\value{
\code{dbern} gives the density, \code{pbern} gives the distribution
function, \code{qbern} gives the quantile function, and \code{rbern}
generates random deviates.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{Binomial}}
}
\keyword{distribution}
