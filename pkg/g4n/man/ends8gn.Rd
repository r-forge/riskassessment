\name{ends8gn}
\alias{ends8gn}
\title{
returns the root and leaf nodes/variables of a gn
}
\description{
From a /gn/ object, the set of nodes/variables is partitioned in four
classes:\cr (i) the root and leaf nodes/variables,\cr (ii) the root
but not leaf nodes/variables,\cr (iii) the leaf but not root
nodes/variables,\cr (iv) the remaining nodes/variables.\cr Of course,
some of the classes can be empty.
}
\usage{
ends8gn(gn)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 ends8gn(g4n.gn7);
}
\arguments{
\item{gn}{The gn.}
}
\details{
No check about \code{gn}
}
\value{
a list of four named numeric vectors with the iin of the
corresponding nodes/variables and the names as names. The four items
are \$RaL (root and leaf), \$RwL (root but not leaf), \$LwR (leaf but
not root) and \$rema (the remaining nodes/variables).
}
