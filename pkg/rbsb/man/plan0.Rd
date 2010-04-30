\name{plan0}
\alias{plan0}
\title{
(bn) generates values from one node of a bn
}
\description{
A vector of N values is extracted from a (most often marginal) node
according to different ways to serve as value(s) when further
generating a simulation design.\cr The possibilities depends on the
node: root, distribution...\cr The result depends also on the nature
of the variable ( numeric or character).
}
\usage{
plan0(bn, ion, N=5, type="U")
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 plan0(rbsb.bn2, 1);
 plan0(rbsb.bn2, 2);
}
\arguments{
\item{bn}{The original bn.}
\item{ion}{indicates the unique node from which values must be
returned (iin or name).}
\item{N}{(=5) number of values to extract.}
\item{type}{(="U") The way to extract the values: "U" for equally
spaced along the plausible range (numeric) "R" for randomly drawn
from the distribution (this applies only for root nodes), "P" for
proportional to the distribution (this applies only for numeric root
nodes and when the distribution is a standard case where R provides
the cumulative distribution.\cr For the moment, only the "U" option
is implemented.}
}
\details{
Notice that the result can be random, so it is advisable to monitor
the pseudo-random seed value. Multivariate nodes are accepted and
their variables are considered as independent nodes.
}
\value{
The resulting vector of values (in fact a matrix with as many columns
as variables in the node, and N rows).
}
