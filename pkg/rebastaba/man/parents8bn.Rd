\name{parents8bn}
\alias{parents8bn}
\title{
returns the direct parents of a bn
}
\description{
(bn) Returns the parents of all nodes or all variables of 'bn'.
}
\usage{
parents8bn(bn, of="n")
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 parents8bn(rebastaba.bn2, "n");
 parents8bn(rebastaba.bn2, "v");
}
\arguments{
\item{bn}{the bn object.}
\item{of}{ 'n' of the nodes; 'v' of the variables.}
}
\value{
A list of character vectors containing the names of the parents, each
component of the list is associated to an item (node or variable).
When there is no parent, character(0) is returned.
}
