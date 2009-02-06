\name{mcprobtree}
\alias{mcprobtree}
\title{
Creates a Stochastic mcnode Object using a Probability Tree
}
\description{
This function builds a mcnode as a mixture of mcstoc functions or
mcnode objects.
}
\usage{
mcprobtree(mcswitch, mcvalues, type=c("V", "U", "VU", "0"), nsv=ndvar(),
	  nsu=ndunc(), nvariates=1, outm="each", seed=NULL)
}
\examples{
## A mixture of normal (prob=0.75), uniform (prob=0.20) and constant (prob=0.05)
conc1 <- mcstoc(rnorm, type="VU", mean=10, sd=2)
conc2 <- mcstoc(runif, type="VU", min=-6, max=-5)
conc3 <- mcdata(0, type="VU")
## Randomly in the cells 
whichdist <- mcstoc(rempiricalD, type="VU", values=1:3, prob= c(.75, .20, .05)) 
mcprobtree(whichdist, list("1"=conc1, "2"=conc2, "3"=conc3), type="VU")
## Which is equivalent to 
mcprobtree(c(.75, .20, .05), list("1"=conc1, "2"=conc2, "3"=conc3), type="VU")
## Not that there is no control on the exact number of occurences.

## Randomly by colums (Uncertainty) 
whichdist <- mcstoc(rempiricalD, type="U", values=1:3, prob= c(.75, .20, .05)) 
mcprobtree(whichdist, list("1"=conc1, "2"=conc2, "3"=conc3), type="VU")

## Randomly by line (Variability) 
whichdist <- mcstoc(rempiricalD, type="V", values=1:3, prob= c(.75, .20, .05)) 
mcprobtree(whichdist, list("1"=conc1, "2"=conc2, "3"=conc3), type="VU")

}
\arguments{
\item{mcswitch}{A vector of probabilities/weights or a mcnode
including the mcstoc functions/mcnodes to pick.}
\item{mcvalues}{A named list of mcnode, mcdata functions or mcstoc
functions, or a combination of those objects. Each element should
lead to an mcnode of type type and of dimension (nsv x nsu x 1) or
(nsv x nsu x nvariates) }
\item{type}{The type of mcnode to be built. By default, a "V" node.
see \code{\link{mcnode}} for details.}
\item{nsv}{The number of simulations in the variability dimension of
the final node.}
\item{nsu}{The number of simulations in the uncertainty dimension of
the final node.}
\item{nvariates}{The number of variates of the final mcnode.}
\item{outm}{The default output of the mcnode for multivariates nodes.
see \code{\link{outm}}.}
\item{seed}{The random seed used for the evaluation. If NULL the seed
is unchanged.}
}
\details{
mcswitch may be: 

\itemize{
\item a vector of length the length of mcvalues. They need not sum to
one, but they should be nonnegative and not all zero. In that case,
each elements of mcvalues will appear in the final sample a random
number of times as specified by this vector. (Note that there is a
random process).
\item a "0" mcnode to build any type of node.
\item a "V" mcnode to build a "V" or a "VU" mcnode.
\item a "U" mcnode to build a "U" or a "VU" mcnode.
\item a "VU" mcnode to build a "VU".}
The elements in mcvalues should be of same type and dimension as
specified in type, nsv, nsu and nvariates. The name should correspond
to the values in mcswitch, specified as character (See Examples).
These elements will be evaluated only if needed : if the
corresponding value is not present in mcswitch, the element will not
be evaluated.
}
\value{
An mcnode object.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{mcdata}}, \code{\link{mcstoc}}, \code{\link{switch}}.
}
\keyword{methods}
