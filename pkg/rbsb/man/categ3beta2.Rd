\name{categ3beta2}
\alias{categ3beta2}
\title{
(ba) compute a bivariate categ distribution
}
\description{
from beta distributions operates an approximate equal spaced
discretization for two variables
}
\usage{
categ3beta2(co=c(1, 1), re=c(1, 1), n=c(3, 4), xnames=list(V.1=form3names(n[1]),
	  V.2=form3names(n[2])))
}
\examples{
 categ3beta2
}
\arguments{
\item{co}{(=c(1,1)) parameters for the common Beta distribution.}
\item{re}{(=c(1,1)) parameters for the Beta distribution associated
to the relationship between the two variables.}
\item{n}{(=c(3,4)) Number of classes to use for the first and second
variable.}
\item{xnames}{(=list(V.1=form3names(n[1]),V.2=form3names(n[2]))) the
dimnames of to apply onto the resulting probability matrix.}
}
\details{
The central idea is the product of two betas onto the domain of the
two variables. The first along the first diagonal (common
distribution), the second along the second diagonal (relationship
distribution) restricted to the corresponding interval. See the code
for more details.
}
\value{
a probability matrix of dimension n
}
\keyword{utilities}
