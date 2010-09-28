\name{num2disc}
\alias{num2disc}
\title{
transforms a numeric value into a categoric value.
}
\description{
A first step towards the discretization of numeric nodes into n
classes
}
\usage{
num2disc(val, lim=0, pod=c("-", "+"))
}
\examples{
 set.seed(1234);
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 num2disc(runif(10)-0.5)
}
\arguments{
\item{val}{vector of value to be discretize.}
\item{lim}{ Vector of the n-1 compulsory limits to define the n
classes. When x == lim[i] it belongs to the ith class.}
\item{pod}{ Vector of the labels associated to the n classes.}
}
\value{
the factor of the resulting categoric values
}
\keyword{utilities}
