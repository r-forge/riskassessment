\name{NA.mcnode}
\alias{NA.mcnode}
\alias{is.na.mcnode}
\alias{is.nan.mcnode}
\alias{is.finite.mcnode}
\alias{is.infinite.mcnode}
\title{
Finite, Infinite, NA and NaN Numbers in mcnode.
}
\description{
\code{is.na}, \code{is.nan}, \code{is.finite} and \code{is.infinite}
return a logical \code{mcnode} of the same dimension as \code{x}.
}
\usage{
\method{is.na}{mcnode}(x)
\method{is.nan}{mcnode}(x)
\method{is.finite}{mcnode}(x)
\method{is.infinite}{mcnode}(x)
}
\examples{
x <- log(mcstoc(rnorm, nsv=1001))
x
is.na(x)



}
\arguments{
\item{x}{A \code{mcnode} object.}
}
\value{
A logical \code{mcnode} object.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{is.finite}}, \code{\link{NA}}
}
\keyword{misc}
