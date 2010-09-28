\name{comp3convolu}
\alias{comp3convolu}
\title{
computes a convoluted distribution
}
\description{
From two probability vector associated to sequential integers
starting from ZERO, returns the probability of the sum.
}
\usage{
comp3convolu(pro1, pro2=pro1)
}
\examples{
 comp3convolu(1:10);
 comp3convolu(comp3convolu(0:5), 0:5);
}
\arguments{
\item{pro1}{Probability vector of the first variable. needs not to be
normalized.}
\item{pro2}{Probability vector of the second variable}
}
\value{
A probability vector of length \code{length(pro1)+length(pro2)-1}.
}
\keyword{iteration}
