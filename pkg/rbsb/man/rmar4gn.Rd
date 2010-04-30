\name{rmar4gn}
\alias{rmar4gn}
\title{
(gn) removes a series of arcs from a gn object
}
\description{
Removing the arcs associated to a subset of nodes, but keeping the
nodes (contrary to rmnd4gn)
}
\usage{
rmar4gn(gn, ion)
}
\examples{
 rsgn3k("RESET"); # needed only for R checking, to be forgotten
 uu <- rbsb.gn1;
 print(uu@arc);
 print(rmar4gn(uu, "A")@arc);
}
\arguments{
\item{gn}{object gn defining the pruned graph}
\item{ion}{vector of nodes the arc of which must be be removed,
either the names or the internal identification.}
}
\value{
The reduced object gn
}
