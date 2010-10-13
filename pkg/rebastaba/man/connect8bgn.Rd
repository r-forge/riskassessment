\name{connect8bgn}
\alias{connect8bgn}
\title{
determines if a bn/gn is connected
}
\description{
(bn) exploring the links between nodes returns the sub-sets of
connected nodes.
}
\usage{
connect8bgn(bgn)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 connect8bgn(rebastaba.bn0);
 connect8bgn(rebastaba.bn3);
 connect8bgn(g4n.gn1);
 connect8bgn(g4n.gn6);
}
\arguments{
\item{bgn}{the /bn/ or /gn/ object to investigate}
}
\value{
A list with two components \code{$nodes} and \code{$group}. Each
component is a named vectors either of the groups to which belongs
each node, or the nodes belonging to each group. For instance,
\code{$nodes} is a vector associated to the nodes indicating the
subsets (or groups). If \code{$nodes[3]} equals 2 this means that the
third \code{iin} node belongs to the second subset. The number of
subsets is then \code{max($nodes)} = \code{length($group)}. The
numbering of the groups is done in decreasing order of their size.
}
\keyword{utilities}
