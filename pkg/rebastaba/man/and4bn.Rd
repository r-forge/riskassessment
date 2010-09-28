\name{and4bn}
\alias{and4bn}
\title{
adds a new node to a bn object
}
\description{
(bn) A new node is added to a bn, it is potentially defined by the
alk argument. \cr The proposed name for the new node must be
different of those already registrated. Also, the parent nodes must
already exist in the bn. This prevents the occurrence of cycles.\cr
The function returns the BN with the added node.\cr For details about
the way to define the node, see the 'new8alk' function.
}
\usage{
and4bn(bn, alk)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print(and4bn(rebastaba.bn1, new8alk(char2des("new"), ltype="normal", 
 lpara=list(mu="{{A}}+{{B}}", sigma=2), lpod=list(c(-1, 6)))), 
 quoi="n");
}
\arguments{
\item{bn}{The bn object to which a node has to be added.}
\item{alk}{An alk object not completed as given by the user.}
}
\details{
The complete list of standard already programmed distributions is
given with help8ltype function.\cr From the definition of the link
given by ("l*" arguments), a function which generates values for the
link is constructed and stored into the slot "lien" of the BN.
}
\value{
an object of class "bn" with an additional node
}
