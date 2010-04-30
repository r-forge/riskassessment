\name{rmnd4gn}
\alias{rmnd4gn}
\title{
(gn) removes a series of nodes from a gn object
}
\description{
Removing the nodes and arcs associated to them, adjusting the slot
arc consequently. Awaiting for the generalization of item in /gn/s,
only node /gn/, i.e. gn@item='n' are accepted here.
}
\usage{
rmnd4gn(gn, nk)
}
\examples{
 rsgn3k("RESET"); # needed only for R checking, to be forgotten
 print(rmnd4gn(rbsb.gn1, 1));
}
\arguments{
\item{gn}{/gn/ object to be modified.}
\item{nk}{vector of number nodes to remove. Must be given in 'nk'
form (see nv2ion).}
}
\details{
the initial @nom slot is reduced.
}
\value{
The reduced object gn
}
