\name{children8bn}
\alias{children8bn}
\title{
(bn) returns the direct children of a bn
}
\description{
Returns the children of all nodes or all variables of 'bn'.
}
\usage{
children8bn(bn, of="n")
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 children8bn(rbsb.bn2, "n");
 children8bn(rbsb.bn2, "v");
}
\arguments{
\item{bn}{the bn object.}
\item{of}{(='n') 'n' of the nodes; 'v' of the variables.}
}
\value{
A list of character vectors containing the names of the children,
each component of the list is associate to an item (node or
variable).
}
