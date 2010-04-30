\name{bn2pta}
\alias{bn2pta}
\title{
(bn) computes the probability table associated to a numcat/parcat
node
}
\description{
In case of a numcat node, provides its conditional probability table
as defined in the bn. The number of dimensions of the array is one
plus the number of the node parents. The order of the node parents is
not modified and the position number of the child node is given by
pos.
}
\usage{
bn2pta(bn, ion, pos=2)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 uu <- and4bn(rbsb.bn1, new8alk(char2des("NC1"), ltype="numcat", 
 lpara=list(p=1:4), lpod=list(LETTERS[1:4])))
 uu <- and4bn(uu, new8alk(char2des("NC2"), ltype="numcat", lparent="NC1", 
 lpara=list(p=matrix(1:12, 4)), lpod=list(letters[1:3])))
 print(bn2pta(uu, "NC1"));
 print(bn2pta(uu, "NC2"));
}
\arguments{
\item{bn}{the bn.}
\item{ion}{ identification of the node. This node must be a numcat
node.}
\item{pos}{(=2) index position for the child node. When there are no
parents, this argument is forced to one.}
}
\details{
Remenber that the order in @npara\$p corresponds to pos with the
greatest value.
}
\value{
the resulting pta object
}
