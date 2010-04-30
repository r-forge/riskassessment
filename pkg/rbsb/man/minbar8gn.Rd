\name{minbar8gn}
\alias{minbar8gn}
\title{
(gn) finds the minimum number of arcs between nodes of a gn object
}
\description{
Finds the minimum number of arcs between nodes of a gn object. To do
so, the parentship is explored and a matrix is returned. This matrix
is similar to the pam matrix but is richer. Its rows and columns are
associated to each node. [i,j] is the minimum number of arcs to
follow to go from node i to node j. When this is not possible, a
value of -1 is introduced.
}
\usage{
minbar8gn(gn)
}
\examples{
 rsgn3k("RESET"); # needed only for R checking, to be forgotten
 minbar8gn(rbsb.gg1);
}
\arguments{
\item{gn}{The gn object to be investigated}
}
\details{
Own nodes are considered as zero distance children (in other words
the diagonal of the resulting matrix is not -1 but 0).
}
\value{
a matrix (see DESCRIPTION field)
}
\keyword{utilities}
