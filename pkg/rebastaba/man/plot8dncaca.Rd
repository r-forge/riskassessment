\name{plot8dncaca}
\alias{plot8dncaca}
\title{
bivariate plot for two categoric variables
}
\description{
(dn) produces an image matrix of the frequencies of the categoric
variables. Several options are possible to give (i) different orders
for the categories, (ii) different viewpoint of the probability table
(joint, conditional) and (iii) different color scales.
}
\usage{
plot8dncaca(xx, rred, ccod, title="", type="joint", ...)
}
\examples{
 rebastaba3k("RESET"); # for R checking convenience
 plot8dncaca(rebastaba.dn4@df[, c(4, 5)], rred=list(rebastaba.dn4@red[[4]], rebastaba.dn4@red[[5]]), ccod=list(rebastaba.dn4@cod[[4]], rebastaba.dn4@cod[[5]]), title="Variables D & E");
}
\arguments{
\item{xx}{ data frame with two columns of the categoric variables to
be plotted}
\item{rred}{list of two components with the ranges of the
representation}
\item{ccod}{list of two components with the common ranges to be
indicated in the diagram.}
\item{title}{(\code{character} The title to be given to the plot.}
\item{type}{ type of representation. "joint" means joint probability;
"row" mean conditional for each row and "column" means conditional
for each column.}
\item{\dots}{Further arguments to be passed to the \code{image}
function.}
}
\value{
returns nothing but a scaled matrix is drawn.
}
