\name{parcours8gn}
\alias{parcours8gn}
\title{
find the ascendances until blocking
}
\description{
exploring the parentship, returns a matrix with rows associated to
nodes giving their ancestors until (if possible) the first blocking
node
}
\usage{
parcours8gn(gn, blocked=character(0), imon=TRUE)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 parcours8gn(g4n.gn7);
}
\arguments{
\item{gn}{The gn object to be dealt}
\item{blocked}{(= character(0)) those nodes which are reference node
for blocking}
\item{imon}{(=TRUE) must blocked node be included?}
}
\details{
In a first step the matrix of chidrenhood is calculated which can be
expansive when the number of nodes is high.
}
\value{
A square matrix. Its rows corresponds to every node. Its columns are
associated to possible ascendants: 0 if not, 1 if it is. The node
itself is not included that is the diagonal elements are zero. The
row and column order are those of iin; the dimnames of this matrix
are given by the node names.
}
