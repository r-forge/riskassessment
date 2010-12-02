\name{path4gn}
\alias{path4gn}
\title{
reduces the graph to the portion in between two node sets
}
\description{
Just removing from the graph those arcs (and nodes) which does not
belong to a path going from a node to another one.\cr As graph
comprising cycles can be introduced, both directions can be
considered.
}
\usage{
path4gn(gn, n1, n2, quel=1, elono=TRUE)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 path4gn(g4n.gn7, "E", "J", 3, FALSE);
 plot(path4gn(g4n.gn7, "A", LETTERS[1:10], 3, FALSE));
}
\arguments{
\item{gn}{The object gn to consider.}
\item{n1}{First node set to consider}
\item{n2}{Second node set to consider}
\item{quel}{(=1) which directions to consider ? 1 for n1 to n2; 2 for
n2 to n1 and 3 for both directions}
\item{elono}{(=TRUE) are eliminated other nodes ?}
}
\details{
To get all descendants of n1, it suffices to put n2 as the complete
set of nodes
}
\value{
The reduced object gn. Notice that if a node belongs to n1 (n2) but
is not linked to any node of n2 (n1) it will be eliminated if elono
is TRUE.
}
