\name{Dirac4bn}
\alias{Dirac4bn}
\title{
introduce a Dirac distribution on one univariate node
}
\description{
(bn) A new bn is generated from the initial bn where the ion-th node
is modified with a Dirac distribution without parents. Then the
structure of the bn can be different. Parameter 'val' gives the value
to provide.
}
\usage{
Dirac4bn(bn, ion, val=rbsb.num0)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print(Dirac4bn(rebastaba.bn2, 1));
 uu <- and4bn(rebastaba.bn1, new8alk(char2des("NC1"), ltype="numcat", 
 lpara=list(p=1:4), lpod=list(LETTERS[1:4])))
 print(uu, quoi="l", qui=4);
 print(Dirac4bn(uu, 4), quoi="l", qui=4);
}
\arguments{
\item{bn}{The original bn.}
\item{ion}{The definition of the node (iin or name vector)}
\item{val}{The value to give to the node. Must be a numeric if the
node is numeric and a character if it is categoric (belonging to the
lpod).\cr Default will imply the mean value of the lcod for numeric
and a median category for categoric (using the order of the
categories).}
}
\value{
The resulting bn
}
