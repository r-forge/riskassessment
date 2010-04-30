\name{plot8dnca}
\alias{plot8dnca}
\title{
(dn) univariate plot for a categoric variable
}
\description{
produces the histogram (raw or ordered) of a categoric variable.
}
\usage{
plot8dnca(f, red, cod, title="", type="ordered", ...)
}
\examples{
 rsdn3k("RESET"); # for R checking convenience
 plot8dnca(rbsb.dn4@df[[5]], red=rbsb.dn4@red[[5]], cod=rbsb.dn4@cod[[5]]);
}
\arguments{
\item{f}{the categoric variable (in fact a factor) to be plotted}
\item{red}{the range of the representation}
\item{cod}{the common limits to be indicated with shaded bars}
\item{title}{(\code{character} The title to be given to the plot.}
\item{type}{ type of representation. "ordered" means that the
categories will be sorted by decreasing importance.}
\item{\dots}{Further arguments to be passed to the \code{hist} or
\code{plot} function. \code{xlim} must not be provided this way since
it is already given by \code{red}.}
}
\value{
returns nothing but one plot is drawn
}
