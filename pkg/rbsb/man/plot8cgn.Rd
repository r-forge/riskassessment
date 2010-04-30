\name{plot8cgn}
\alias{plot8cgn}
\title{
(gn) draws nodes and arcs of a gn with already computed xy values
}
\description{
Internal drawing of plot.gn, isolated because it can be used for
multiple plots. The plot is supposed to be already open and the
coordinates of nodes already prepared. See plot8gn for the details
about decorations.
}
\usage{
plot8cgn(gn, xy)
}
\arguments{
\item{gn}{The gn to plot}
\item{xy}{Matrix of (x,y) (nbn x 2) coordinates for the nodes.}
}
\details{
The reason of xy besides the @pos of the gn object is to give the
possibility of different projections with the same positions.
}
\value{
returns nothing but something is drawn on the open plot
}
