\name{position4gn}
\alias{position4gn}
\title{
proposes positions for the nodes of a gn object
}
\description{
Following some heuristics computes new positions for a /gn/
}
\usage{
position4gn(gn, heur="a")
}
\arguments{
\item{gn}{gn object}
\item{heur}{(='a') indicates which heuristic must be used. (a) for
acp on the arc, (h) adapted for hierarchical trees from roots, (h2)
idem but with vertical alternance, (H) adapted for hierarchical trees
from leafs, (H2) idem but with vertical a}
}
\value{
The same 'gn' object with the new positions
}
