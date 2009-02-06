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
is.na, is.nan, is.finite and is.infinite return a logical mcnode of
the same dimension as x.
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
\item{x}{A mcnode object.}
}
\value{
A logical mcnode object.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{is.finite}}, \code{\link{NA}}
}
\keyword{NA}
