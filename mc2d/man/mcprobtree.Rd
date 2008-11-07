\name{mcprobtree}
\alias{mcprobtree}
\title{
Creates a Stochastic mcnode Object using a Probability Tree
}
\description{
If one node should be built using a mixed of \code{mcstoc}
functions/nodes, \code{mcprobtree} creates this \code{\link{mcnode}}
object from one node specifying the \code{mcstoc} functions/nodes to
pick and a list of the different \code{mcstoc} functions/nodes.
}
\usage{
mcprobtree(mcswitch, mcvalues, type=c("V", "U", "VU", "0"), nsv=ndvar(),
	  nsu=ndunc(), nvariates=1, outm="each", seed=NULL)
}
\examples{
conc1 <- mcstoc(rnorm, type="U", mean=10, sd=2)
conc2 <- mcstoc(runif, type="U", min=8, max=12)
whichdist <- mcstoc(rbern, type="U", prob=0.25) 
concbis <- mcprobtree(whichdist, list("0"=conc1, "1"=conc2), type="VU")

}
\arguments{
\item{mcswitch}{A \code{mcnode} including the \code{mcstoc}
functions/mcnodes to pick.}
\item{mcvalues}{A named list of \code{mcnode}, \code{mcdata}
functions or \code{mcstoc} functions, or a combination of those
objects.}
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
\item a \code{"0"} mcnode to build any type of node.
\item a \code{"V"} mcnode to build a \code{"V"} or a \code{"VU"}
mcnode.
\item a \code{"U"} mcnode to build a \code{"U"} or a \code{"VU"}
mcnode.
\item a \code{"VU"} mcnode to build a \code{"VU"}.}
 \code{mcswitch} will be recycled correctly if needed. (see
\code{\link{mcdata}})

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
