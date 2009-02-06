\name{plot.mc}
\alias{plot.mc}
\alias{plot.mcnode}
\alias{plot.plotmc}
\alias{plot.mccut}
\title{
Plots Results of a Monte Carlo Simulation
}
\description{
Plots the empirical cumulative distribution function of a mcnode or a
mc object ("0" and "V" nodes) or the empirical cumulative
distribution function of the estimate of a mcnode or mc object ("U"
and "VU" nodes).
}
\usage{
\method{plot}{mc}(x, prec=0.01, stat=c("median", "mean"), lim=c(0.025, 0.975),
	  na.rm=TRUE, griddim=NULL, xlab=NULL, ylab="Fn(x)", main="",
	  draw=TRUE, ...)
\method{plot}{mcnode}(x, ...)
\method{plot}{plotmc}(x, ...)
\method{plot}{mccut}(x, stat=c("median", "mean"), lim=c(0.025, 0.975), griddim=NULL,
	  xlab=names(x), ylab="Fn(x)", main="", draw=TRUE, ...)
}
\examples{
data(total)
plot(xVUM3)
plot(total)



}
\arguments{
\item{x}{a mcnode or a mc objects}
\item{prec}{the precision of the plot. 0.01 will provide an ecdf from
the 0.00, 0.01, .02, ..., 1.00 quantiles, 0.001 will provide a 0.000,
0.001, 0.002, ..., 1.000 quantiles,... }
\item{stat}{the function used for estimates (2D mc or mcnode). By
default the median.}
\item{lim}{a vector of numbers (between 0 and 1) indicating the
enveloppe (2D mc or mcnode) . Maybe NULL or empty.}
\item{na.rm}{Should NA values be discarded}
\item{griddim}{a vector of two integers, indicating the size of the
grid of the graph. If NULL, the grid is calculated to produce a
"nice" graph.}
\item{xlab}{vector of labels for the x-axis. If NULL, use the name of
the node.}
\item{ylab}{vector of labels for the y-axis.}
\item{main}{vector of main titles of the graph.}
\item{draw}{Should the plot be drawn?}
\item{\dots}{further arguments to be passed to plot.ecdf.}
}
\details{
plot.mcnode is a user-friendly function that send the mcnode to
plot.mc.

 For "VU" and "U" mcnodes, quantiles are calculated using
\code{\link{quantile.mc}} within each of the nsu simulations (i.e. by
columns of each mcnode). The medians (but may be the means using
stat="mean") calculated from the nsu values are plotted. The 0.025
and 0.975 quantiles (default values of lim) of these quantiles are
used as the enveloppe.
}
\value{
A plot.mc object, list of the quantiles used to plot the draw.
}
\references{
Cullen AC and Frey HC (1999) Probabilistic techniques in exposure
assessment. Plenum Press, USA, pp. 81-155.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{ecdf}}, \code{\link{plot}}, \code{\link{quantile.mc}}
}
\keyword{hplot}
