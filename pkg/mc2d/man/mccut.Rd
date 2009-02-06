\name{mccut}
\alias{mccut}
\alias{evalmccut}
\alias{print.mccut}
\alias{mcmodelcut}
\title{
Evaluates a Two-Dimensional Monte Carlo Model in a Loop.
}
\description{
evalmccut evaluates a Two-Dimensional Monte Carlo model using a loop
on the uncertainty dimension. Within each loop, it calculates
statistics in the variability dimension and stores them for further
analysis. It allows to evaluate very high dimension models using
(unlimited?) time instead of (limited) memory.

 mcmodelcut builds a mcmodelcut object that can be sent to evalmccut.
}
\usage{
evalmccut(model, nsv=ndvar(), nsu=ndunc(), seed=NULL, ind="index")
\method{print}{mccut}(x, lim=c(0.025, 0.975), digits=3, ...)
mcmodelcut(x, is.expr=FALSE)
}
\examples{
modEC3 <- mcmodelcut({

## First block:
## Evaluates all the 0, V and U nodes.
 { cook <- mcstoc(rempiricalD, type = "V", values = c(0, 1/5, 
 1/50), prob = c(0.027, 0.373, 0.6))
 serving <- mcstoc(rgamma, type = "V", shape = 3.93, rate = 0.0806)
 conc <- mcstoc(rnorm, type = "U", mean = 10, sd = 2)
 r <- mcstoc(runif, type = "U", min = 5e-04, max = 0.0015)
 }
## Second block:
## Evaluates all the VU nodes
## Leads to the mc object. 
 {
 expo <- conc * cook * serving
 dose <- mcstoc(rpois, type = "VU", lambda = expo)
 risk <- 1 - (1 - r)^dose
 res <- mc(conc, cook, serving, expo, dose, r, risk)
 }
## Third block:
## Leads to a list of statistics: summary, plot, tornado
## or any function leading to a vector (et), a list (minmax), 
## a matrix or a data.frame (summary)
 {
 list(
 sum = summary(res), 
 plot = plot(res, draw=FALSE), 
 minmax = lapply(res, range), 
 et = sapply(res, sd)
 )
 }
})

evalmccut(modEC3, nsv = 101, nsu = 101, seed = 666)


}
\arguments{
\item{model}{a mcmodelcut object obtained using mcmodelcut function
or (directly) a valid call including three blocks. See Details and
Examples for the structure of the call.}
\item{x}{a call or an expression (if is.expr=TRUE) including three
blocks. See Details and Examples for the structure of the call.}
\item{nsv}{The number of simulations for variability used in the
evaluation.}
\item{nsu}{The number of simulations for uncertainty used in the
evaluation.}
\item{seed}{The random seed used for the evaluation. If NULL the seed
is unchanged.}
\item{ind}{The variable name used in model to refers to the
uncertainty. see Details and Example.}
\item{is.expr}{ FALSE to send a call, TRUE to send an expression (see
\code{\link{mcmodel}} examples)}
\item{lim}{A vector of values used for the quantile function
(uncertainty dimension).}
\item{digits}{Number of digits in the print.}
\item{\dots}{Additional arguments to be passed in the final print
function.}
}
\details{
This function should be used for high dimension Two-Dimensional
Monte-Carlo simulations, when the memory limits of \R are attained.
The use of a loop will take (lots of) time, but less memory.

 x (or model if a call is used directly in evalmccut) should be built
as three blocks. 

\enumerate{
\item The first block is evaluated once (and only once) before the
first loop (step 1).
\item The second block, which should lead to an mc object, is
evaluated using nsu = 1 (step 2).
\item The third block is evaluated on the mc object. All resulting
statistics are stored (step 3).
\item The steps 2 and 3 are repeated nsu times. At each iteration,
the values of the loop index (from 1 to nsu) is given to the variable
specified in ind.
\item Finally, the nsu statistics are returned in an invisible object
of class mccut.
\item The first block (maybe empty) is an expression that will be
evaluated only once. This block should evaluate all "V" mcnode and
"0" mcnodes. It may evaluate "U" mcnodes that will be sent in the
second and third block by column, and, optionnaly, some other codes
(even "VU" mcnode, sent by columns) that can not be evaluated if
ndunc==1 (e.g. sampling without replacement in the uncertainty
dimension).
\item The second block is an expression that leads to the mc object.
It must end with an expression as mymc \eqn{<}- mc(...). The variable
specified as ind may be helpful to refer to the uncertainty dimension
in this step 
\item The last block should build a list of statistics refering to
the mc object. The function summary should be used if a summary, a
tornado on uncertainty (\code{\link{tornadounc.mccut}}) or a
convergence diagnostic \code{\link{converg}} is needed, the function
\code{\link{plot.mc}} should be used if a plot is needed, the
function \code{\link{tornado}} should be used if a tornado is needed.
Moreover, any other function that leads to a vector, a matrix, or a
list of vector/matrix of statistics evaluated from the mc object may
be used. list are time consuming.}
 IMPORTANT WARNING: do not forget to affect the results, since the
print method provide only a summary of the results while all data may
be stored in an mccut object.
}
\value{
An object of class mccut. This is a list including statistics
evaluated within the third block. Each list consists of all the nsu
values obtained. The print.mccut method print the median, the mean,
the lim quantiles estimated on each statistics on the uncertainty
dimension.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{evalmcmod}}
}
\note{
The methods and functions available on the mccut object is function
of the statistics evaluated within the third block: 

\itemize{
\item a \code{\link{print.mccut}} is available as soon as one
statistic is evaluated within the third block;
\item a \code{\link{summary.mccut}} and a
\code{\link{tornadounc.mccut}} are available if a
\code{\link{summary.mc}} is evaluated within the third block;
\item \code{\link{converg}} may be used if a \code{\link{summary.mc}}
is evaluated within the third block;
\item a \code{\link{plot.mccut}} is available if a
\code{\link{plot.mc}} is evaluated within the third block. (Do not
forget to use the argument draw = FALSE in the third block);
\item a \code{\link{tornado}} is available if a \code{\link{tornado}}
is evaluated within the third block.}
The seed is set at the beginning of the evaluation. Thus, the
complete similarity of two evaluations is not certain, depending of
the structure of your model. Moreover, with a similar seed, the
simulation will not be equal to the one obtained with
\code{\link{evalmcmod}} since the random samples will not be obtained
in the same order.

 In order to avoid conflicts between the model evaluation and the
function, the function uses upper case variables. Do not use upper
case variables in your model.

 The function should be re-adapted if a new function to be applied on
mc objects is written.
}
\keyword{methods}
