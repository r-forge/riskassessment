\name{zero2bn}
\alias{zero2bn}
\title{
(bn) starting an empty bn
}
\description{
Creation of a new bn object with zero nodes, that is just giving the
structure to be able to add nodes further.
}
\usage{
zero2bn(description)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(zero2bn(char2des("BN without any node")));
}
\arguments{
\item{description}{Description field ('des' object) for the new bn.
Alternatively can be the name (single character).}
}
\details{
The argument can be no more than a single character, in that case a
'des' object will be created with it as name.
}
\value{
an object of class "bn" with zero node
}
