\name{print8bn}
\alias{print8bn}
\title{
(bn) prints a bn object
}
\description{
prints different aspects of a bn object.
}
\usage{
print8bn(x, ..., quoi="tDn", qui=rbsb.cha0, comment="n", quoi.des="nd",
	  quoi.alk="d")
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(rbsb.bn2);
}
\arguments{
\item{x}{The bn to be printed}
\item{\dots}{Further arguments to be passed to the standard print
function.}
\item{quoi}{ This character string indicates what must be printed. It
can comprise:\cr "a" for all\cr "s" for standard\cr "D" for
description of the bn\cr "t" for title\cr "n" for nodes\cr "P" for
ascendants and descendants at the node level\cr "p" for ascendants
and descendants at the variable level\cr "v" for variables\cr "d" for
description of the nodes\cr "l" for definition of the links\cr "g"
for generating fonction of each node}
\item{qui}{ The nodes which must be considered for the printing.
rbsb.cha0 implies all nodes, if not either a character vector
providing the nodes or a numeric vector with the iin (internal
numbers of them). Using this argument is a way to modify the order of
displaying the information about the nodes when printing. E.g.
qui=length(x@nom):1 will present the node in decreasing order. No
selection is (for the moment) possible at the variable level.}
\item{comment}{(="n") This character string indicates how it must be
printed. It can comprise:\cr "n": natural order based on @nom,\cr
"a": alphabetical order on the node/variab}
\item{quoi.des}{ what to print when printing the bn description}
\item{quoi.alk}{ what to print when printing a node. Not used for the
moment.}
}
\details{
When two incompatible specifications are ordered into "comment"
argument, the used one is the first in the description list.\cr
rbsb.msi could be increased to see the parentship without limitation.
}
\value{
nothing but some printing is issued
}
\keyword{print}
