\name{categ3beta}
\alias{categ3beta}
\title{
compute a univariate categ distribution
}
\description{
(ba) from a beta(a,b) distribution operates an approximate equal
spaced discretization with n classes
}
\usage{
categ3beta(a=1, b=1, n=3, xname=form3names(n))
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 categ3beta(a=1, b=9, n=5);
}
\arguments{
\item{a}{ First parameter for the Beta distribution.}
\item{b}{ Second parameter for the Beta distribution.}
\item{n}{ Number of classes to use.}
\item{xname}{ names to provide for the probability vector.}
}
\value{
a probability vector of size n
}
\keyword{utilities}
