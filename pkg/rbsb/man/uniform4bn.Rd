\name{uniform4bn}
\alias{uniform4bn}
\title{
(bn) introduces a uniform distribution on one node
}
\description{
A new bn is generated from the initial bn where the ion-th node is
modified with a uniform distribution without parents. Then the
structure of the bn can be different.
}
\usage{
uniform4bn(bn, ion, val=rbsb.num0)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(uniform4bn(rbsb.bn2, 1));
 print(uniform4bn(rbsb.bn1, "B"));
}
\arguments{
\item{bn}{The original bn.}
\item{ion}{The definition of the node (iin or name)}
\item{val}{(\code{numeric}) Defines the range of the uniform
distribution. For a numeric node it is a numeric(2); for a categoric
node, it is a character(p). Be aware that the lpod are not modified
from the initial bn. The default uses the lpod (see the DETAILS
section). In case of multivariate or repeated node, when the default
is not used, val must be a matrix with as many rows as variables, the
first column for the lower limit of the uniform and the second column
for the upper limit of the uniform.}
}
\details{
If numeric, the limits for the uniform are taken from lpod (or lred
if infinite). If categoric, the limits are numcat uniform onto
lpod.\cr This modification of node is possible only for univariate
nodes.
}
\value{
The resulting bn
}
