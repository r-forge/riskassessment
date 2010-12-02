\name{mnd4bn}
\alias{mnd4bn}
\title{
modifies an existing node into a bn object
}
\description{
(bn) An already existing node is replaced into a bn. Some of its
characteristics cannot be modified: name, lvar and lnat. This is a
condition for not to modify the other nodes.\cr The user must take
great care that the effect of default is NOT AT ALL as in the similar
function and4bn ! When adding a new node, the default is nothing,
when modifying an existing node the default is NO MODIFICATION except
for lfunct, ldaf... Some checks are performed.\cr For the moment
repeated node cannot be replaced.
}
\usage{
mnd4bn(bn, ion, alk)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print(mnd4bn(rebastaba.bn1, 2, new8alk(char2des("B"), ltype="uniform", 
 lpara=list(a="{{A}}", b="2*{{A}}"), 
 lpod=list(c(-10, 10))
 )));
}
\arguments{
\item{bn}{The bn object to which a node has to be modified.}
\item{ion}{The node to replace, by internal number or name.}
\item{alk}{Definition of the replacing node by the user.}
}
\details{
More insights are given in the description of and4bn.
}
\value{
The modified bn
}
