\name{gn2path}
\alias{gn2path}
\title{
returns all paths between two nodes
}
\description{
To return all paths between two nodes, this function prepares the
parentship matrix and calls the recursive function pam2path
}
\usage{
gn2path(gn, d, a)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 gn2path(g4n.gn7, "A", "F");
}
\arguments{
\item{gn}{a gn object}
\item{d}{departure node (ion)}
\item{a}{arrival node (ion)}
}
\value{
a list of vectors with all possible paths (nodes are identified by
iins)
}
