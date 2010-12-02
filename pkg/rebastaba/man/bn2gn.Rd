\name{bn2gn}
\alias{bn2gn}
\title{
creates a gn object from a bn object
}
\description{
(bn) from a bn object extracts the necessary information to create a
gn object (the directed graph of the BN). According to 'nv', the
graph can be built at the node or variable level.
}
\usage{
bn2gn(bn, nv="n")
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print(bn2gn(rebastaba.bn1));
}
\arguments{
\item{bn}{the bn object}
\item{nv}{ 'n' for node level, 'v' for variable level.}
}
\value{
the resulting gn object
}
