\name{rgn}
\alias{rgn}
\title{
creates a pseudo-random gn object
}
\description{
Creates a gn object which characteristics are randomly generated
following the requirements of the call. Also positions illuminating
the struture are proposed. Normally the produced gn is connected and
without cycles. When the request is not possible, a warning is issued
and some compatible option is provided. Using this function with the
same set.seed starting point (and the same request) must produce
identical gn.\cr Different types of gn are possible, their
characteristics are given by arguments and pseudo-randomness. The
type argument describes the structure of the links without care about
the directions (as edges). The possible types are:\cr 

\describe{
\item{chain}{All nodes are aligned in a simple chain. So a
relationship exists between nno and nar: nno = nar +1. Then nno will
be drawn and nar deduced.}
\item{ring}{Like the chain type but a cycle is done with the
relationships and nno = nar.}
\item{hier}{A hierarchy in the sense that the nodes can be disposed
in a dendrogram structure; a particular case of this is the "star"
structure when there only a branching point and the branches can
comprise several nodes.}
\item{audit}{A hierarchy with two levels. For this specific case,
'nno' is the number of grouping levels and 'nar' is the number of
terminal nodes for each grouping levels. Names of nodes are
determined accordingly.}
\item{square}{Not so simple to describe. Nodes are placed in the cell
of square matrix and edges between nodes respects this structure.
Arcs (including their number) are deduced from the node positions.
Run some cases and you will understand seing the plot.}
\item{tetra}{Four of the nodes are on the vertices of a tetrahedron
and the other nodes are on its edges.}
\item{total}{The maximum number of arcs, nno*(nno-1)/2, is provided.
So nar will not be considered.}}
 It is suggested that the seed be initialized before calling this
function.
}
\usage{
rgn(des, type="hier", nno=c(5, 10), nar=c(10, 20), nom=LETTERS,
	  dire="nat", lev=1)
}
\examples{
 set.seed(1234);
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 r1 <- rgn(char2des("r1"), type="hier");
 r2 <- rgn(char2des("r2"), type="square");
}
\arguments{
\item{des}{Either the name or the description (preferably) for the
newly created gn.}
\item{type}{type of the gn, see the description field for details}
\item{nno}{ range within which must be the number of nodes. Of course
the range can be null, in that case the number of nodes is directly
provided by the user. If not, it is drawn according to a uniform
distribution (one integer from \code{nno[1]} to \code{nno[2]}).\cr
Single values are repeated, e.g \code{5} becomes \code{c(5,5)}. When
audit, it gives the number of grouping nodes.}
\item{nar}{ range within which must be the number of arcs. Similar to
\code{nno} but \code{nno} being fixed \code{nar} must satisfy some
constraints. These constraints may also depend on the \code{type} of
/gn/.\cr When audit, it gives the number of nodes for each grouping
nodes.\cr Single values are repeated, e.g \code{5} becomes
\code{c(5,5)}.}
\item{nom}{vector of names to be used for the nodes.}
\item{dire}{ indicates the directions of the arcs between the nodes
of a /gn/. There are two possibilities: "nat" for natural directions
and "ran" for random under the restriction of no cycles.\cr For
'audit' the two choices are 'nat' for putting the first node as root
else it will be the synthesis.\cr No effect when type ="ring".}
\item{lev}{ maximum level of hierarchy; only used when \code{type} is
"hier".}
}
\details{
Of course if too much arcs are ordered, the probability of a
non-cycle graph may be very small, even null.\cr To prevent this, a
local nb\_trial limits the number of draws, if this limit is reached
then a fatal error is issued.\cr For some type (e.g. "square"), the
ordered number of arcs and nodes may not be respected for some
reasons.
}
