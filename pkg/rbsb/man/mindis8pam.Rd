\name{mindis8pam}
\alias{mindis8pam}
\title{
(ba) finds the minimum distance between every pair of nodes
}
\description{
From the parentship matrix, the minimum distance matrix between every
pairs of nodes is computed. [i,j] means from node ith to node jth. In
most cases (especially for non empty dags) the matrix is not
symmetric.\cr The distance (not a mathematical distance) is the
minimal number of arcs to follow before reaching the node j starting
from the node i.
}
\usage{
mindis8pam(pam)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 mindis8pam(rbsb.pam1);
}
\arguments{
\item{pam}{The pam matrix to be investigated}
}
\details{
-1 are introduced on the diagonal.
}
\value{
The matrix containing the distances
}
\keyword{utilities}
