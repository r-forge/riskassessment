\name{unmc}
\alias{unmc}
\title{
Unclasses the mc or the mcnode Object
}
\description{
Unclasses the \code{mc} object in a list of arrays or the
\code{mcnode} object in an array.
}
\usage{
unmc(x, drop=TRUE)
}
\examples{
data(total)
## A vector
unmc(total$xV, drop=TRUE)
## An array
unmc(total$xV, drop=FALSE)
}
\arguments{
\item{x}{A \code{mc} or a \code{mcnode} object.}
\item{drop}{Should the dimensions of size 1 be dropped (see
\code{\link{drop}}.}
}
\value{
if x is an \code{mc} object: a list of arrays. If \code{drop=TRUE}, a
list of vectors, matrixes and arrays. if x is an \code{mcnode}
object: an array. If \code{drop=TRUE}, a vector, matrix or array.
}
\author{
Regis Pouillot
}
\keyword{manip}
