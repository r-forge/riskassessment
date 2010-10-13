\name{print8pta}
\alias{print8pta}
\title{
prints the node-variable names
}
\description{
(ba) prints the node-variable names of x (a 'nom' object)
}
\usage{
print8pta(x, ..., comment=11)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 print(new("pta", name="Null pta", 
 vam="A", vac=character(0), 
 vad=character(0), vav=character(0), 
 kkk=2, 
 pro=array(1:3, dim=3, dimnames=list(A=c("a", "b", "c")))
 ));
}
\arguments{
\item{x}{nom object}
\item{\dots}{Further arguments to be passed to the print function.}
\item{comment}{ Numeric scalar defining the way to print 0: just the
probability table as an array 1: like 0 plus indications about
marginal, conditional and conditioned dimensions. 10: just the
probability table as data frame (each line associated to the second
dimension of the probability array) 11: like 10 plus indications
about marginal, conditional and conditioned dimensions. }
}
\details{
First probabilities are normalized to sum 1 over all marginale (this
is to be consistent with the multinomial parameterizations in most R
functions.) Then probabilities are printed using the scaling slot
@kkk. More precisely, if kkk \eqn{>} 0 then
round(p*10\eqn{\mbox{\textasciicircum}}{^}kkk,0) is printed if kkk =
0 then round(p,3) is printed if kkk \eqn{<} 0 then round(p,-kkk) is
printed
}
\value{
nothing a printing is issued
}
\keyword{print}
