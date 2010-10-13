\name{plot8dncoco}
\alias{plot8dncoco}
\title{
bivariate plot for two continuous variables
}
\description{
(dn) produces a Cartesian plot (\code{points}) or an estimated
density into R\eqn{\mbox{\textasciicircum}}{^}2 (\code{contour}).
}
\usage{
plot8dncoco(xx, rred, ccod, title="", type="points", lla=NULL, ...)
}
\examples{
 rebastaba3k("RESET"); # for R checking convenience
 plot8dncoco(rebastaba.dn4@df[, 1:2], rred=list(rebastaba.dn4@red[[1]], rebastaba.dn4@red[[2]]), ccod=list(rebastaba.dn4@cod[[1]], rebastaba.dn4@cod[[2]]), xlab="A", ylab="B");
}
\arguments{
\item{xx}{ matrix or data frame with two columns of the two
continuous variable to be plotted}
\item{rred}{list of two components with the ranges of the
representation}
\item{ccod}{list of two components with the common limits to be
indicated with vertical dotted lines}
\item{title}{(\code{character} The title to be given to the plot.}
\item{type}{ type of representation. "points" means a scatter plot
with symbols, "labels" means scatter plot with text labels and
"contour" means the isocontour plot.}
\item{lla}{ Defines the labels for the represented points. NULL for
standard, If scalar must be the number of c}
\item{\dots}{Further arguments to be passed to the \code{plot} or
\code{contour} function. \code{xlim} must not be provided this way
since it is already given by \code{rred}.}
}
\value{
returns nothing but one plot is drawn
}
