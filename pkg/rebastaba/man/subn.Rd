\name{subn}
\alias{subn}
\title{
finds independent sub-bn in a BN
}
\description{
(bn) /// FOR THE MOMENT DOES NOT WORK PROPERLY /// exploring the
parentship, returns an order on the nodes, such that generating the
nodes following this order produces a draw from the joint
distribution. \cr By the way describes the independent subsets of
nodes and stops with a message when a cycle is suspected.
}
\usage{
subn(bn)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 cat("To be added\n");
}
\arguments{
\item{bn}{The bn object}
}
\value{
a list with 

\item{subsets}{independent subsets indicated by successive numbers
with respect to iin order. Its max is then the number of independent
subsets.}
\item{order}{a compatible order to the calculation of the joint
probability. It is given with respect to the iin order. It follows
the independent subsets.}
}
