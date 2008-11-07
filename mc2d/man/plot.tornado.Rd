\name{plot.tornado}
\alias{plot.tornado}
\alias{plot.tornadounc}
\title{
Draws a Tornado chart.
}
\description{
Draws a Tornado chart as provided by \code{tornado}.
}
\usage{
\method{plot}{tornado}(x, which=1, name=NULL, stat=c("median", "mean"), xlab="method",
	  ylab="", ...)
\method{plot}{tornadounc}(x, which=1, stat=2, name=NULL, xlab="method", ylab="", ...)
}
\arguments{
\item{x}{A \code{\link{tornado}} object or a \code{\link{tornadounc}}
object.}
\item{which}{Which output to print -for multivariates output-.}
\item{name}{Vector of name of input variables. If NULL, the name will
be given from the name of the elements.}
\item{stat}{The name (or the number of column) of the statistics of
the output to be considered.}
\item{xlab}{Label of the x axis. if "method", use the correlation
method used in the \code{tornado} object.}
\item{ylab}{Label of the y axis.}
\item{\dots}{Further arguments to be passed to the \code{plot}
function.}
}
\details{
A point is drawn at the estimate and the segment reflects the
uncertainty around this estimate.
}
\value{
NULL
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{tornado}}
}
\keyword{hplot}
