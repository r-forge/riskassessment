\name{parents8bn}
\alias{parents8bn}
\title{
(bn) returns the direct parents of a bn
}
\description{
Returns the parents of all nodes or all variables of 'bn'.
}
\usage{
parents8bn(bn, of="n")
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 parents8bn(rbsb.bn2, "n");
 parents8bn(rbsb.bn2, "v");
}
\arguments{
\item{bn}{the bn object.}
\item{of}{(='n') 'n' of the nodes; 'v' of the variables.}
}
\value{
A list of character vectors containing the names of the parents, each
component of the list is associate to an item (node or variable).
When there is no parent, character(0) is returned.
}
