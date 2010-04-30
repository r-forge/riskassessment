\name{range8node}
\alias{range8node}
\title{
(bn) returns the variable ranges of a given node
}
\description{
The ranges can have different acception, see the 'range' argument.
}
\usage{
range8node(bn, ion, range="standard")
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 range8node(rbsb.bn2, 1);
 range8node(rbsb.bn1, 3);
}
\arguments{
\item{bn}{The bn in which the node is defined}
\item{ion}{indicates the unique node to be considered.}
\item{range}{(="standard") the lpod interval if finite else the lred
interval. Other possibilities are "lpod","lred","lcod".}
}
\details{
When the variable is not numeric, 'standard' is synonymous with
'lpod'.
}
\value{
a list (as many components as variables) with the corresponding
domain. When the variable is numeric, intervals are given as
numeric(2), the lower bound being the first.
}
