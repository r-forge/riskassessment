\name{bn2gn}
\alias{bn2gn}
\title{
(bn) creates a gn object from a bn object
}
\description{
from a bn object extracts the necessary information to create a gn
object (the directed graph of the BN). According to 'nv', the graph
can be built at the node or variable level.
}
\usage{
bn2gn(bn, nv="n")
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(bn2gn(rbsb.bn1));
}
\arguments{
\item{bn}{the bn object}
\item{nv}{(='n') 'n' for node level, 'v' for variable level.}
}
\value{
the resulting gn object
}
