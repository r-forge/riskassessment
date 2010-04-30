\name{print8pgr}
\alias{print8pgr}
\title{
(ba) prints the node-variable names
}
\description{
prints the node-variable names of x (a 'nom' object)
}
\usage{
print8pgr(x, ..., what="v", type=0)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 print(new("pgr", 
 unitscale=20, 
 padding=1.1, 
 cexscale=1, 
 arrowlength=0.12, 
 sscale=1, 
 kzoom=1.2, 
 diar=0.05));
}
\arguments{
\item{x}{nom object}
\item{\dots}{Further arguments to be passed to the print function.}
\item{what}{(='v') what to print ('v'=node-variable names; 'n' only
no}
\item{type}{(=0) type of printing (0) a node a line; (1) everything
on the same line.}
}
\value{
nothing a printing is issued
}
\keyword{print}
