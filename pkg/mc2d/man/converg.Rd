\name{converg}
\alias{converg}
\title{
Graph of Running Statistics in the Variability or in the Uncertainty
Dimension.
}
\description{
This function provides basic graphs to evaluate the convergence of a
node of a \code{\link{mc}} or a \code{\link{mccut}} object in the
variability or in the uncertainty dimension. 
}
\usage{
converg(x, node=length(x), margin=c("var", "unc"), nvariates=1, iter=1,
	  probs=c(0.025, 0.975), lim=c(0.025, 0.975), griddim=NULL,
	  log=FALSE)
}
\examples{
data(total)
converg(xVU, margin="var")
converg(xVU, margin="unc")
}
\arguments{
\item{x}{A \code{\link{mcnode}} object, a \code{\link{mc}} object or
a \code{\link{mccut}} object.}
\item{node}{The node to be considered in a mc object or a mccut
object, displayed either as the order number or the name of the node.
By default: the last node of the object.The corresponding node should
not be of type "0" in a mc object or of type "0" or "V" in a mccut
object.}
\item{margin}{The margin used to plot the graph. margin is used only
if the node is a "VU" mcnode.}
\item{nvariates}{The variates to be considered. nvariates is used
only for multivariates nodes.}
\item{iter}{If margin == "var" and the node is a "VU" mcnode, iter
specify the iteration in the uncertainty dimension to be used for the
graph.}
\item{probs}{The quantiles to be provided in the variability
dimension.}
\item{lim}{The quantiles to be used in the uncertainty dimension.}
\item{griddim}{A vector of two integers, indicating the size of the
grid of the graph. If NULL, the grid is calculated to produce a
"nice" graph.}
\item{log}{If TRUE, the data will be log transformed.}
}
\details{
If the node is of type "V", the running mean, median and probs
quantiles according to the variability dimension will be provided. If
the node is of type "VU" and margin=="var", this graph will be
provided on one simulation in the uncertainty dimension (chosen by
iter).

 If the node is of type "U" the running mean, median and lim
quantiles according to the uncertainty dimension will be provided.

 If the node is of type "VU" (with margin=="unc" or from a mccut
object), one graph are provided for each of the mean, median and
probs quantiles calculated in the variability dimension.
}
\author{
Regis Pouillot
}
\note{
This function may be used on a mccut object only if a summary.mc
function was used in the third block of the \code{\link{evalmccut}}
call. The values used as probs arguments in converg should have been
used in the summary.mc function of this third block.
}
\keyword{hplot}
