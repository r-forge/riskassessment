\name{dmultinomial}
\alias{dmultinomial}
\alias{rmultinomial}
\title{
The Vectorized Multinomial Distribution
}
\description{
Generate multinomially distributed random number vectors and compute
multinomial probabilities.
}
\usage{
dmultinomial(x, size=NULL, prob, log=FALSE)
rmultinomial(n, size, prob)
}
\examples{
prob <- c(1, 2, 7)
rmultinomial(4, 1000, prob)
rmultinomial(4, c(10, 100, 1000, 10000), prob)

(prob <- matrix(c(1, 2, 7, 3, 3, 3, 7, 2, 1), ncol=3, byrow=TRUE))
rmultinomial(3, 1000, prob)
rmultinomial(4, c(10, 100, 1000, 10000), prob)

}
\arguments{
\item{x}{Vector of length K of integers in 0:size.}
\item{n}{Number of random vectors to draw.}
\item{size}{A vector of integers, say N, specifying the total number
of objects that are put into K boxes in the typical multinomial
experiment. For \code{dmultinom}, it defaults to \code{sum(x)}. The
first element correspond to the vector \code{prob} or the first row
of \code{prob}, ...}
\item{prob}{Numeric non-negative vector of length K, or matrix of
size \code{x x K} specifying the probability for the K classes; is
internally normalized to sum 1.}
\item{log}{Logical; if TRUE, log probabilities are computed.}
}
\details{
This function is the vectorized version of \code{\link{rmultinom}}
and \code{\link{dmultinom}}. Recycling is permitted.
}
\keyword{distribution}
