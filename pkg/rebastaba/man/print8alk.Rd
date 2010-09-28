\name{print8alk}
\alias{print8alk}
\title{
prints the node-variable names
}
\description{
(ba) prints the node-variable names of x (a 'nom' object)
}
\usage{
print8alk(x, ..., retrait=4, proba=FALSE)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 print(new("alk", ldes=new("des", name="AA"), 
 ltype="normal", 
 lpara=list(mu=0, sigma=1), 
 lrep=0, 
 lnat="conti", 
 lvar="", lparent=character(0), 
 lpod=list(c(-3, 3)), 
 ltransfo=character(0), 
 ldaf=rbsb.daf0, 
 lwin=rebastaba.win0, 
 lcomp=FALSE));
}
\arguments{
\item{x}{nom object}
\item{\dots}{Further arguments to be passed to the print function.}
\item{retrait}{ number of spaces for indentation.}
\item{proba}{ Must the probability table be printed? (only for numcat
nodes)}
}
\value{
nothing a printing is issued
}
\keyword{print}
