\name{mcmodel}
\alias{mcmodel}
\title{
Monte Carlo model
}
\description{
Specify a \code{mcmodel}, without evaluating it, for a further
evaluation using \code{\link{evalmcmod}}.
}
\usage{
mcmodel(x, is.expr=FALSE)
}
\examples{
modEC1 <- mcmodel({
 conc <- mcdata(10, "0")
 cook <- mcstoc(rempiricalD, values=c(0, 1/5, 1/50), prob=c(0.027, 0.373, 0.600))
 serving <- mcstoc(rgamma, shape=3.93, rate=0.0806)
 expo <- conc * cook * serving
 dose <- mcstoc(rpois, lambda=expo)
 risk <- 1-(1-0.001)^dose
 mc(conc, cook, serving, expo, dose, risk)
 })
evalmcmod(modEC1, nsv=100, nsu=100)
}
\arguments{
\item{x}{An \R call or an expression.}
\item{is.expr}{ \code{FALSE} to send a call, \code{TRUE} to send an
expression (see examples)}
}
\details{
The model should be put between \code{\{\dots\}} and the last line
should be of the form \code{mc(...)}. Any reference to the number to
the number of simulation in the dimension of variability (2D model)
should be done via \code{ndvar()} or (preferred) \code{nsv}. Any
reference to the number of simulations in the dimension of
uncertainty should be done via \code{ndunc()} or (preferred)
\code{nsu}.
}
\value{
an \R expression, with class \code{mcmodel}
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{expression}}.

 \code{\link{evalmcmod}} to evaluate the model.

 \code{\link{mcmodelcut}} to evaluate high Dimension Monte Carlo
Model in a loop.
}
\keyword{programming}
