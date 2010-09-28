\name{and4gn}
\alias{and4gn}
\title{
adds a series of nodes to a gn object
}
\description{
This function adds a series of nodes to a /gn/ without any links
giving them the names into argument 'names'. They are unidimensional
with empty variable names.
}
\usage{
and4gn(gn, names=1, pos=NULL)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 and4gn(g4n.gn7, c("U", "V", "W")); 
}
\arguments{
\item{gn}{The object gn to be completed}
\item{names}{(=1) when numeric of length 1, it indicates the number
of nodes to add (and the name will be automatically determined by
form3names function. When character it is the vector of the new names
and its length gives the number of node to add.}
\item{pos}{(=NULL) gives the positions to add. When null, they will
be automatically provided. When a matrix, it must comprise as many
rows as new nodes and 2 (x,y) or 3 (x,y,z) columns.}
}
\details{
With respect to the internal order, the new nodes are introduced at
the end.
}
\value{
The completed object gn
}
