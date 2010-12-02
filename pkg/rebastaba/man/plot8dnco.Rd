\name{plot8dnco}
\alias{plot8dnco}
\title{
univariate plot for a continuous variable
}
\description{
(dn) produces the histogram (raw or smoothed) of a continuous
variable.
}
\usage{
plot8dnco(x, red, cod, title="", type="smooth", ...)
}
\examples{
 rebastaba3k("RESET"); # for R checking convenience
 plot8dnco(rebastaba.dn4@df[[1]], red=rebastaba.dn4@red[[1]], cod=rebastaba.dn4@cod[[1]]);
}
\arguments{
\item{x}{the continuous variable to be plotted}
\item{red}{the range of the representation}
\item{cod}{the common limits to be indicated with vertical dotted
lines}
\item{title}{(\code{character} The title to be given to the plot.}
\item{type}{ type of representation. "smooth" means that
\code{density} is called else a simple histogram is drawn.}
\item{\dots}{Further arguments to be passed to the \code{hist} or
\code{plot} function. \code{xlim} must not be provided this way since
it is already given by \code{red}.}
}
\value{
returns nothing but one plot is drawn
}
