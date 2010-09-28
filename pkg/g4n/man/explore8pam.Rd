\name{explore8pam}
\alias{explore8pam}
\title{
finds different characteristics of relations between nodes
}
\description{
(ba) exploring the parentship matrix 'pam', returns a list of
different nodesxnodes matrices giving standard characteristics of the
relationship between them.\cr The calculation is done with iin order
and the result is proposed in iin order. The relation are: (i) the
indication of the relation (-1=row is descendant, 0= none, 1= row is
ascendant), (ii) the minimum number of arcs to join row and column (0
means no) (iii) the maximum number of arcs to join row and column (0
means no) (iv) the number of different paths joining row and column
}
\usage{
explore8pam(pam, iir=bc(nrow(pam@rlt)), jjc=iir)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 explore8pam(g4n.pam1);
}
\arguments{
\item{pam}{The pam object}
\item{iir}{ the subset of nodes to be considered as starting nodes.
They will be en rows of the resulting matrices.}
\item{jjc}{ the subset of nodes to be considered as ending nodes.
They will be in columns of the resulting matrices.}
}
\value{
a list gathering two lists \$des: a list of descriptions explaining
what is computed in the differents matrices \$rel: the list of the
computed matrices
}
\keyword{utilities}
