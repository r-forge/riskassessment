\name{is.mc}
\alias{is.mc}
\alias{is.mcnode}
\title{
Tests mc and mcnode Objects
}
\description{
\code{is.mc} tests \code{mc} objects and \code{is.mcnode} tests
\code{mcnode} objects.
}
\usage{
is.mc(x)
is.mcnode(x)
}
\examples{
data(total)
is.mcnode(xVU)
is.mcnode(total)
is.mc(total)

}
\arguments{
\item{x}{An \code{mc} or a \code{mcnode} object.}
}
\details{
\code{is.mc} tests if \code{x} is a list of \code{mcnode}, each
elements being of compatible dimension. It tests if the class
\code{"mc"} is affected to the object.

 \code{is.mcnode} tests if \code{x} is an array of numeric or
logical, if it has a "type" attribute and compatible dimensions, and
if the class \code{"mcnode"} is affected to the object.
}
\value{
\code{TRUE} or \code{FALSE}
}
\author{
Regis Pouillot
}
\keyword{utilities}
