\name{print8nom}
\alias{print8nom}
\title{
prints the node-variable names
}
\description{
prints the node-variable names of x (a 'nom' object)
}
\usage{
print8nom(x, ..., what="v", type=0)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 print(rbsb.nom2);
 print(rbsb.nom2, what='nv');
}
\arguments{
\item{x}{nom object}
\item{\dots}{Further arguments to be passed to the print function.}
\item{what}{ what to print ('v'=node:variable names;
'nv'=node[variable] names; 'n' only no}
\item{type}{ type of printing: (0) everything on the same line; (1) a
node, a line.}
}
\value{
nothing a printing is issued
}
\keyword{print}
