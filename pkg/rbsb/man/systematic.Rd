\name{systematic}
\alias{systematic}
\title{
returns a systematic exploration of a vector
}
\description{
returns a vector of indices of \code{x} such that this is explored in
systematic way by \code{nb} draws, spaced of \code{kk} according to
its order.\cr Even is \code{ord} is not "random", a call to
\code{sample} is done so it is advised to call first \code{set.seed}.
}
\usage{
systematic(x, nb, kk, ord="random")
}
\examples{
 rbsb3k("reset"); # only necessary for R checking
 systematic(1:20, 5, 1);
 systematic(1:20, 5, 3);
 systematic(1:20, 15, 3);
 systematic(1:20, 15, 3, "order");
 systematic(1:20, 15, 3, "random");
 sort(systematic(1:20, 55, 3));
 uu <- c(2*(1:10), 1+2*(0:9));
 uu[systematic(uu, 20, 3)];
}
\arguments{
\item{x}{ The vector to systematically sample.}
\item{nb}{ desired number of draws.}
\item{kk}{ frequency of successive draws. Something giving
quasi-uniform draw is is \code{min(length(x)-1,length(x)\%
/\%
(nb\%
\%
length(x)))} ???notice that the direct application of this formula
failed for rounding phenomenum???.}
\item{ord}{indicates which type of ordering must be done on the
output: \code{randor} for a circular random permutation;
\code{random} for a complete random permutation; \code{order} for a
sorting with \code{sort}; \code{such} for nothing (in that case, it
depends on the algorithm which can be }
}
\details{
When \code{nb*kk} is less than \code{length(x)} then the sequence is
1, kk+1, 2*kk+1,..., (nb-1)*kk+1 taking into account the ordered
order of values of \code{x}. If not either look at the code or
experiment the function.
}
\value{
A vector of length \code{nb} of indices of \code{x}.
}
\keyword{misc}
