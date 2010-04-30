\name{nd2bn}
\alias{nd2bn}
\title{
(bn) extracts the sub-bn comprising a node and its direct parents
}
\description{
From a given bn and for one its node (defined by ion), the bn
comprising this node and its direct parent is extracted. Every parent
is given a uniform distribution.
}
\usage{
nd2bn(bn, ion)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(nd2bn(rbsb.bn1, 3));
 print(nd2bn(rbsb.bn2, "B"));
 print(nd2bn(rbsb.bn2, 1));
}
\arguments{
\item{bn}{The original bn.}
\item{ion}{The definition of the target node.}
}
\details{
The two limits used for the uniform distribution of the parents are
taken from their @lpod or @lred attributes. If these limits are not
convenient, they can easily being modified with mnd4bn function
afterwards. When there are more than one parent, the uniform are
independent. See also uniform4bn for the description of uniform in
case of multivariate and repeated nodes.
}
\value{
The resulting bn.
}
