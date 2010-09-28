\name{rnd4bn}
\alias{rnd4bn}
\title{
removes a series of nodes into a bn object
}
\description{
(bn) Some of the nodes of a bn are removed. A condition is that no
orphans must be generated.
}
\usage{
rnd4bn(bn, ion)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print(rnd4bn(rebastaba.bn1, 3));
 print(rnd4bn(rebastaba.bn1, 2:3));
}
\arguments{
\item{bn}{The bn object to which some nodes will be removed.}
\item{ion}{The nodes to remove, designated by their internal numbers
or names.}
}
\value{
The modified bn
}
