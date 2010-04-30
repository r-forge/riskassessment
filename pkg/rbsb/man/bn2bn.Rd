\name{bn2bn}
\alias{bn2bn}
\title{
(bn) extracts a bn from a bn (way number 1)
}
\description{
A new bn is generated from the initial bn reduced to the targetted
nodes indicated in 'ion' and the parents of these nodes. Independent
uniform distributions are given to the parents AND AT THE SAME TIME
THEIR OWN RELATIONSHIPS ARE DELETED. For some argument about this
practice, see the DETAILS section of function study8nd.\cr To give an
example, the graph (A-\eqn{>}B; A-\eqn{>}C; B-\eqn{>}C) will result
into (A-\eqn{>}C; B-\eqn{>}C) if (C) is the targetted node.
}
\usage{
bn2bn(bn, ion)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(bn2bn(rbsb.bn2, "A"));
}
\arguments{
\item{bn}{The original bn.}
\item{ion}{The definition of the nodes (iin or name vector)}
}
\value{
The resulting bn
}
