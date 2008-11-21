\name{dimmcnode}
\alias{dimmcnode}
\alias{dimmc}
\title{
Dimension of mcnode and mc Objects
}
\description{
Provides the dimension (i.e. the number of simulations in the
variability dimension, the number of simulations in the uncertainty
dimension and the maximum number of variates of a \code{mcnode} or a
\code{mc} object.
}
\usage{
dimmcnode(x)
dimmc(x)
}
\examples{
data(total)
dimmcnode(xVUM2)
dimmc(total)

}
\arguments{
\item{x}{a \code{mcnode} or a \code{mc} object.}
}
\value{
A vector of three scalars: the dimension of variability (1 for
\code{"0"} and \code{"U" mcnode}), the dimension of uncertainty (1
for \code{"0"} and \code{"V" mcnode}) and the number of variates (the
maximal number of variates for an \code{mc} object.
}
\author{
Regis Pouillot
}
\note{
This function does not test if the object is correctly built. See
\code{\link{is.mcnode}} and \code{\link{is.mc}} .
}
\keyword{utilities}
