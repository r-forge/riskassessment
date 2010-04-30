\name{anode4bn}
\alias{anode4bn}
\title{
(bn) very simplified version of and4bn
}
\description{
In this simplified version of and4bn, the user has no more than
giving the name and the parents of the node. The distribution will be
automatically put to 'Dirac' and the parameter value as the sum of
the parents...
}
\usage{
anode4bn(bn, des, parent=character(0))
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(anode4bn(rbsb.bn2, "sup"));
 print(anode4bn(rbsb.bn2, "sup", c("A", "B")));
}
\arguments{
\item{bn}{bn object to which a node has to be added}
\item{des}{either the name or a description ('des' object).}
\item{parent}{(=character(0)) Character vector with the names of the
parents. character(0) means there is no parents.}
}
\details{
Behind, a standard call to and4bn is made
}
\value{
an object of class "bn" with an additional node
}
