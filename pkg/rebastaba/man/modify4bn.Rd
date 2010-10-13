\name{modify4bn}
\alias{modify4bn}
\title{
plot an object rbsb to interactively modify its graph
}
\description{
(bn) Issues a graphical display of the network and using the mouse,
the user can change the position of the nodes, add and remove arcs,
rotate the graph to modify the projection plane.\cr See modify4gn for
further details.
}
\usage{
modify4bn(bn, pg=g4n.pgr0)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 \dontrun{modify4bn(rebastaba.bn2);}
}
\arguments{
\item{bn}{the rbsb object to plot}
\item{pg}{graphics parameters (see rebastaba.la)}
}
\details{
Everything is done through bn2gn, modify4gn and gn2bn
}
\value{
returns the modified bn
}
