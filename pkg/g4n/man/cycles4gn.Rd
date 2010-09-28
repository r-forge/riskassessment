\name{cycles4gn}
\alias{cycles4gn}
\title{
reduces the graph to the portion with cycles
}
\description{
Just removing from the graph those nodes (and associated arcs) which
cannot belong to a cycle because they are extreme (either without
parent or without children).\cr
}
\usage{
cycles4gn(gn)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 cycles4gn(g4n.gn7); # returns the null /gn/
}
\arguments{
\item{gn}{object gn defining the graph}
}
\value{
The reduced object gn
}
