\name{mcprobtree}
\alias{mcprobtree}
\title{
Creates a Stochastic mcnode Object using a Probability Tree
}
\description{
This function builds a \code{mcnode} as a mixture of \code{mcstoc}
functions or \code{mcnode} objects.
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
whichdist <- mcstoc(rempiricalD, type="VU", values=1:3, prob= c(.75, .25, .05)) 
mcprobtree(whichdist, list("1"=conc1, "2"=conc2, "3"=conc3), type="VU")
mcprobtree(c(.75, .20, .05), list("1"=conc1, "2"=conc2, "3"=conc3), type="VU")

## Randomly by colums (Uncertainty) 
whichdist <- mcstoc(rempiricalD, type="U", values=1:3, prob= c(.75, .25, .05)) 
mcprobtree(whichdist, list("1"=conc1, "2"=conc2, "3"=conc3), type="VU")

## Randomly by line (Variability) 
whichdist <- mcstoc(rempiricalD, type="V", values=1:3, prob= c(.75, .25, .05)) 
mcprobtree(whichdist, list("1"=conc1, "2"=conc2, "3"=conc3), type="VU")

}
\arguments{
\item{mcswitch}{A vector of probabilities/weights or a \code{mcnode}
including the \code{mcstoc} functions/mcnodes to pick.}
\item{mcvalues}{A named list of \code{mcnode}, \code{mcdata}
functions or \code{mcstoc} functions, or a combination of those
objects. Each element should lead to an \code{mcnode} of type
\code{type} and of dimension \code{c(nsv x nsu x 1)} or \code{c(nsv x
nsu x nvariates)} }
\item{type}{The type of \code{mcnode} to be built. By default, a
\code{"V"} node. see \code{\link{mcnode}} for details.}
\item{nsv}{The number of simulations in the variability dimension of
the final node.}
\item{nsu}{The number of simulations in the uncertainty dimension of
the final node.}
\item{nvariates}{The number of variates of the final \code{mcnode}.}
\item{outm}{The default output of the \code{mcnode} for multivariates
nodes. see \code{\link{outm}}.}
\item{seed}{The random seed used for the evaluation. If \code{NULL}
the \code{seed} is unchanged.}
}
\details{
\code{mcswitch} may be: 

\itemize{
\item a vector of length the length of \code{mcvalues}. They need not
sum to one, but they should be nonnegative and not all zero. In that
case, each elements of \code{mcvalues} will appear in the final
sample a random number of times as specified by this vector.
\item a \code{"0"} mcnode to build any type of node.
\item a \code{"V"} mcnode to build a \code{"V"} or a \code{"VU"}
mcnode.
\item a \code{"U"} mcnode to build a \code{"U"} or a \code{"VU"}
mcnode.
\item a \code{"VU"} mcnode to build a \code{"VU"}.}
The elements in \code{mcvalues} should be of same type and dimension
as specified in \code{type}, \code{nsv}, \code{nsu} and
\code{nvariates}. The name should correspond to the values in
\code{mcswitch}, specified as character (See Examples). These
elements will be evaluated only if needed : if the corresponding
value is not present in \code{mcswitch}, the element will not be
evaluated.
}
\value{
An \code{mcnode} object.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{mcdata}}, \code{\link{mcstoc}}, \code{\link{switch}}.
}
\keyword{methods}
