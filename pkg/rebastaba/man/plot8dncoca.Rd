\name{plot8dncoca}
\alias{plot8dncoca}
\title{
bivariate plot for a continuous and a categoric variables
}
\description{
(dn) produces a series of univariate diagram of the continuous
variables for each category of the categoric variable
}
\usage{
plot8dncoca(xx, rred, ccod, title="", type="smooth", ...)
}
\examples{
 rebastaba3k("RESET"); # for R checking convenience
 par(mfrow=c(2, 1));
 plot8dncoca(rebastaba.dn4@df[, c(1, 4)], rred=list(rebastaba.dn4@red[[1]], rebastaba.dn4@red[[4]]), ccod=list(rebastaba.dn4@cod[[1]], rebastaba.dn4@cod[[4]]), xlab="A", ylab="density", title="Variable D");
}
\arguments{
\item{xx}{ data frame with two columns of the continuous variable (in
first position) and the categoric variable (in second position) to be
plotted}
\item{rred}{list of two components with the ranges of the
representation}
\item{ccod}{list of two components with the common ranges to be
indicated in the diagram.}
\item{title}{(\code{character} The title to be given to the plot.}
\item{type}{ type of representation. "smooth" means density plots
else histograms are drawn.}
\item{\dots}{Further arguments to be passed to the \code{plot} or
\code{hist} function. \code{xlim} must not be provided this way since
it is already given by \code{rred[[1]]}.}
}
\value{
returns nothing but a series of diagrams (as many as representation
categories of the second variable) is drawn.
}
