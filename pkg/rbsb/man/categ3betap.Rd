\name{categ3betap}
\alias{categ3betap}
\title{
(ba) compute a p-variable categ distribution
}
\description{
from beta distributions generates the joint probability table for p
(=length(n)) categoric variables coming from the discretization of
the betas (approximately equal spaced discretized).
}
\usage{
categ3betap(co=c(1, 1), re=c(2, 2), n=c(3, 4, 5), xnames=rbsb.cha0)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 categ3betap();
}
\arguments{
\item{co}{(=c(1,1)) First parameter when calling categ3beta2.}
\item{re}{(=c(2,2)) Second parameter when calling categ3beta2.}
\item{n}{(=c(3,4,5)) Number of classes to use for the p variables.
Its length provides the number of variables.}
\item{xnames}{(=rbsb.cha0) dimnames of the resulting probability
array. The default generates some standard names.}
}
\details{
The central idea is the product of pairs of beta obtained from
categ3beta2. For instance, in case of p=3, if P1(i,j) is associated
to the first two variables, P2(i,k) is associated to the first and
last variables \& P3(j,k) is associated to the last two variables,
then the joint proba is P(i,j,k) propto P1(i,j)P2(i,k)P3(j,k).
}
\value{
a probability array of dim n
}
\keyword{utilities}
