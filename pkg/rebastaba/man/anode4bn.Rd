\name{anode4bn}
\alias{anode4bn}
\title{
very simplified version of and4bn
}
\description{
(bn) In this simplified version of and4bn, the user has no more than
giving the name and the parents of the node. The distribution will be
automatically put to 'Dirac' and the parameter value as the sum of
the parents which cannot comprise more than a variable.
}
\usage{
anode4bn(bn, des, parent=character(0))
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print(anode4bn(rebastaba.bn2, "sup"));
 print(anode4bn(rebastaba.bn2, "sup", c("A", "B[b]")));
}
\arguments{
\item{bn}{bn object to which a node has to be added}
\item{des}{either the name (\code{character(1)} or a description
(/des/).}
\item{parent}{ Character vector with the names of the parents.
\code{character(0)} means there is no parents.}
}
\details{
Behind, a standard call to and4bn is made
}
\value{
an object of class "bn" with one additional node.
}
