\name{unmc}
\alias{unmc}
\title{
Unclasses the mc or the mcnode Object
}
\description{
Unclasses the mc object in a list of arrays or the mcnode object in
an array.
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
\item{x}{A mc or a mcnode object.}
\item{drop}{Should the dimensions of size 1 be dropped (see
\code{\link{drop}}).}
}
\value{
if x is an mc object: a list of arrays. If drop=TRUE, a list of
vectors, matrixes and arrays. if x is an mcnode object: an array. If
drop=TRUE, a vector, matrix or array.
}
\author{
Regis Pouillot
}
\keyword{manip}
