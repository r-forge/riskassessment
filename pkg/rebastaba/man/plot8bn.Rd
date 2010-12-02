\name{plot8bn}
\alias{plot8bn}
\title{
plots an object bn
}
\description{
(bn) produces the plot of the graph of the BN x
}
\usage{
plot8bn(x, y="useless", ..., frame=FALSE)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 \dontrun{plot(rebastaba.bn2);}
}
\arguments{
\item{x}{the /bn/ object to plot}
\item{y}{ Useless argument introduced for compatibility with generic
plot functions of R.}
\item{\dots}{Further arguments to be passed to the standard plot
function.}
\item{frame}{ must a frame be added?}
}
\details{
The constant g4n.pgr0 is used to build the graph
}
\value{
returns nothing but a plot is drawn
}
