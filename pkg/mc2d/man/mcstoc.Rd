\name{mcstoc}
\alias{mcstoc}
\title{
Creates Stochastic mcnode Objects
}
\description{
Creates a \code{\link{mcnode}} object using a random generating
function.
}
\usage{
mcstoc(func=runif, type=c("V", "U", "VU", "0"), ..., nsv=ndvar(),
	  nsu=ndunc(), nvariates=1, outm="each", nsample="n",
	  seed=NULL, rtrunc=FALSE, linf=-Inf, lsup=Inf, lhs=FALSE)
}
\examples{
Oldnvar <- ndvar()
Oldnunc <- ndunc()
ndvar(5)
ndunc(4)

## compatibility with mcdata as arguments
x0 <- mcstoc(runif, type="0")
xV <- mcstoc(runif, type="V")
xU <- mcstoc(runif, type="U")
xVU <- mcstoc(runif, type="VU")

## "0" accepts mcdata "0"
mcstoc(runif, type="0", min=-10, max=x0)

## "V" accepts "0" mcdata and "V" mcdata
mcstoc(rnorm, type="V", mean=x0, sd=xV)

## "U" accepts "0" mcdata and "U" mcdata
mcstoc(rnorm, type="U", mean=x0, sd=xU)

## "VU" accepts "0" mcdata, "U" mcdata
## "V" mcdata and "U" mcdata with correct recycling
mcstoc(rnorm, type="VU", mean=x0, sd=xVU)
mcstoc(rnorm, type="VU", mean=xV, sd=xU)

## any function giving a set (vector/matrix) of value of length 'size' works
f <- function(popi) 1:popi
mcstoc(f, type="V", nsample="popi")

##Multivariates

ndvar(2)
ndunc(5)
##Build a multivariate node with univariate distribution
mcstoc(rnorm, "0", nvariates=3)
mcstoc(rnorm, "V", nvariates=3)
mcstoc(rnorm, "U", nvariates=3)
mcstoc(rnorm, "VU", nvariates=3)

##Build a multivariate node with multivariates distribution
alpha <- mcdata(c(1, 1000, 10, 100, 100, 10, 1000, 1), "V", nvariates=4)
(p <- mcstoc(rdirichlet, "V", alpha=alpha, nvariates=4))
mcstoc(rmultinomial, "VU", size=10, p, nvariates=4)

##Build a univariates node with "multivariates" distribution
size <- mcdata(c(1:5), "U")
mcstoc(rmultinomial, "VU", size, p, nvariates=1) #since a multinomial return one value

##Build a multivariates node with "multivariates" distribution
mcstoc(rmultinomial, "VU", size, p, nvariates=4) #sent 4 times to fill the array

##Use of rempiricalD with nodes
##A bootstrap
ndunc(5)
ndvar(5)
dataset <- c(1:9)
(b <- mcstoc(rempiricalD, "U", nvariates=9, values=dataset))
unclass(b)
##Then we build a VU node by sampling in each set of bootstrap
(node <- mcstoc(rempiricalD, "VU", values=b))
unclass(node)

## truncated
ndvar(2)
ndunc(5)
linf <- mcdata(-1:3, "U")
x <- mcstoc(rnorm, "VU", rtrunc=TRUE, linf=linf)
unclass(round(x))
linf <- mcdata(1:5, "U")
mcstoc(rnorm, "VU", nsv=100, rtrunc=TRUE, linf=linf, lhs=TRUE)

ndvar(Oldnvar)
ndunc(Oldnunc)
}
\arguments{
\item{func}{A function providing random data or its name as
character.}
\item{type}{The type of \code{mcnode} to be built. By default, a
\code{"V"} node. see \code{\link{mcnode}} for details.}
\item{\dots}{All other arguments but the size of the sample to be
passed to \code{func}. These arguments should be vectors or
\code{mcnode}s (arrays prohibited).}
\item{nsv}{The number of simulations in the variability dimension.}
\item{nsu}{The number of simulations in the uncertainty dimension.}
\item{nvariates}{The number of variates of the output.}
\item{outm}{The output of the \code{mcnode} for multivariates nodes.
May be "each" (default) if an output should be provided for each
variates considered independently, "none" for no output or a vector
of functions (as a character string) that will be applied on the
variates dimension before any output (ex: "mean", "median",
c("min","max")). Each function should return 1 value when applied to
1 value (ex. do not use \code{"range"}). Note that the \code{outm}
attribute may be changed further using the \code{\link{outm}}
function.}
\item{nsample}{The name of the parameter of the function giving the
size of the vector. By default, \code{n}, as in most of the random
sampling distributions of the \code{stats} library (with the
exceptions of \code{rhyper} and \code{rwilcox} where
\code{nsample="nn"} should be used).}
\item{seed}{The random seed used for the evaluation. If \code{NULL}
the \code{seed} is unchanged.}
\item{rtrunc}{Should the distribution be truncated? See
\code{\link{rtrunc}}.}
\item{linf}{If truncated: lower limit. May be a scalar, an array or a
mcnode.}
\item{lsup}{If truncated: upper limit. May be a scalar, an array or a
mcnode.}
\item{lhs}{Should a Random Latin Hypercube Sampling be used? see
\code{\link{lhs}}}
}
\details{
Note that arguments after \dots must be matched exactly.

Any function who accepts vectors/matrix as arguments may be used
(notably: all current random generator of the \code{stats} package).
The arguments may be sent classically but it is strongly recommended
to use consistant \code{mcnode}s if arguments should be recycled,
since a complex recycling is handled for \code{mcnode} and not for
vectors. The rules for compliance of \code{mcnode} arguments are as
following (see below for special functions): 

\describe{
\item{type="V"}{accepts \code{"0" mcnode} of dimension \code{(1 x 1 x
nvariates)} or of dimension \code{(1 x 1 x 1)} (recycled) and
\code{"V" mcnode} of dimension \code{(nsv x 1 x nvariates)} or
\code{(nsv x 1 x 1)} (recycled).}
\item{type="U"}{accepts \code{"0" mcnode} of dimension \code{(1 x 1 x
nvariates)} or of dimension \code{(1 x 1 x 1)} (recycled) and
\code{"U" mcnode} of dimension \code{(1 x nsu x nvariates)} or of
dimension \code{(1 x nsu x 1)} (recycled).}
\item{type="VU"}{accepts \code{"0" mcnode} of dimension \code{(1 x 1
x nvariates)} or of dimension \code{(1 x 1 x 1)} (recycled),
\code{"V" mcnode} of dimension \code{(nsv x 1 x nvariates)} (recycled
classicaly) or \code{(nsv x 1 x 1)} (recycled classically), \code{"U"
mcnode} of dimension \code{(1 x nsu x nvariates)} (recycled by rows)
or \code{(1 x nsu x 1)} (recycled by row on the uncertainty dimension
and classicaly on variates), \code{"VU" mcnode} of dimension
\code{(nsv x nsu x nvariates)} or of dimension \code{(nsv x nsu x 1)}
(recycled).}
\item{type="0"}{accepts \code{"0" mcnode} of dimension \code{(1 x 1 x
nvariates)} or \code{(1 x 1 x 1)} (recycled).}}
Multivariate nodes and multivariate distributions:

The number of variates should be provided (not guesses by the
function). A multivariates node may be built using a univariate
distribution and \code{nvariates!=1}. See examples.

\code{\link{rdirichlet}} needs for \code{alpha} a vector or a
multivariates nodes and returns a multivariate node.
\code{\link{rmultinomial}} needs for \code{size} and \code{prob}
vectors and/or multivariate nodes and return a univariate or a
multivariate node. \code{\link{rmultinormal}} needs for \code{mean}
and \code{sigma} vectors and/or multivariate nodes and return a
multivariate node. \code{\link{rempiricalD}} needs for \code{values}
and \code{prob} vectors and/or multivariate nodes and return a a
univariate or a multivariate node. See examples.

\code{trunc=TRUE} is valid for univariates distributions only. The
distribution will be truncated on \code{[linf, lsup]}. The function
'func' should have a 'q' form (with first argument 'p') and a 'p'
form, as all current random generator of the \code{stats} library.
Example : 'rnorm' (has a 'qnorm' and a 'pnorm' form), 'rbeta',
'rbinom', 'rgamma', ...

If \code{lhs=TRUE}, a Random Hypercube Sampling will be used on
\code{nsv} and \code{nsu} The function 'func' should have a 'q' form
(with argument 'p'). \code{lhs=TRUE} is thus not allowed on
multivariates distributions.
}
\value{
An \code{mcnode} object.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{mcnode}} for a description of \code{mcnode} object,
methods and functions on \code{mcnode} objects.

 \code{\link{Ops.mcnode}} for operations on \code{mcnode} objects.
}
\keyword{methods}
