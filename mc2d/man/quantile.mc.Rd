\name{quantile.mc}
\alias{quantile.mc}
\alias{quantile.mcnode}
\title{
Quantiles of a mc Object
}
\description{
Evaluates quantiles of a mc object. This function is used by
\code{plot.mc}
}
\usage{
\method{quantile}{mc}(x, probs=seq(0, 1, 0.01), lim=c(0.025, 0.975), na.rm=TRUE, ...)
\method{quantile}{mcnode}(x, ...)
}
\examples{
data(total)
quantile(total$xVUM3)
quantile(total)

}
\arguments{
\item{x}{a \code{mc} objects}
\item{probs}{the quantiles to be calculated}
\item{na.rm}{TRUE or FALSE}
\item{lim}{a vector of numbers (between 0 and 1) indicating the
enveloppe. Maybe \code{NULL} or empty.}
\item{\dots}{For generic method consistancy.}
}
\value{
A list of quantiles. For 1D objects (\code{mcnode "0D","1D","2DV"}),
the corresponding element is a vector of quantiles. For 2D objects
(\code{mcnode "2D","1D","2DV"}), the corresponding element is a
\code{length(lim)+2} rows matrix (includes median, mean and
\code{lim} quantiles in the uncertainty dimension.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{plot.mc}}, \code{\link{quantile}}.
}
\keyword{univar}
