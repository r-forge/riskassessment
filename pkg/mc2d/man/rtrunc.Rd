\name{rtrunc}
\alias{rtrunc}
\title{
Random Truncated Distributions
}
\description{
Provides samples from classical \R distributions and \pkg{mc2d}
specific distributions truncated between linf and lsup.
}
\usage{
rtrunc(distr=runif, n, linf=-Inf, lsup=Inf, ...)
}
\examples{
rtrunc("rnorm", n=10, linf=0)
range(rtrunc(rnorm, n=1000, linf=3, lsup=5, sd=10))
}
\arguments{
\item{distr}{A function providing random data or its name as
character. The function 'rdistr' should have a 'qdistr' form (with
argument 'p') and a 'pdistr' form (with argument 'q'). Example :
'rnorm' (has a 'qnorm' and a 'pnorm' form), 'rbeta', 'rbinom',
'rgamma', ...}
\item{n}{The size of the sample.}
 .

\item{linf}{A vector of lower bounds.}
\item{lsup}{A vector of upper bounds.}
\item{\dots}{All arguments to be passed to pdistr and qdistr.}
}
\details{
The function 1) evaluates the p values corresponding to linf and lsup
using pdistr; 2) samples n values using \code{runif(n, min=pinf,
max=psup)}, and 3) takes the n corresponding quantiles from the
specified distribution using qdistr.

All distributions (but sample) implemented in the stats library could
be used. The arguments in \dots should be named. Do not use 'log' or
'log.p' or 'lower.tail'.
}
\value{
A vector of n values.
}
\author{
Regis Pouillot
}
\note{
The inversion of the quantile function leads to time consuming
functions for some distributions.
}
\keyword{distribution}
