\name{conti2categ}
\alias{conti2categ}
\title{
transforms continuous values into a categoric values.
}
\description{
Transforms continuous values into a categoric values. A first step
towards the discretization of numeric variables into n classes
}
\usage{
conti2categ(val, lim=0, pod=c("-", "+"))
}
\examples{
 set.seed(1234);
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 conti2categ(runif(10)-0.5);
 conti2categ(runif(10), seq(0.1, 0.9, length=9), LETTERS[1:10]);
}
\arguments{
\item{val}{vector of values to be discretized.}
\item{lim}{ Vector of the n-1 compulsory limits to define the n
classes. When x == lim[i] it belongs to the ith class.}
\item{pod}{ Vector of the labels associated to the n classes. Its
length provides the number of classes.}
}
\value{
the \code{character} of the resulting categoric values
}
\keyword{utilities}
