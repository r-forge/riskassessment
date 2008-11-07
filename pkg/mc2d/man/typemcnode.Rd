\name{typemcnode}
\alias{typemcnode}
\title{
Provides the Type of a mcnode Object
}
\description{
Provide the type of a \code{mcnode} object from its class.
}
\usage{
typemcnode(x, index=FALSE)
}
\examples{
data(total)
typemcnode(total$xVUM2)
}
\arguments{
\item{x}{a \code{mcnode} object}
\item{index}{if \code{TRUE} give the index of the type rather than
the type.}
}
\value{
\code{"0", "V","U" or "VU"} or the corresponding index if
\code{index=TRUE}.

 \code{NULL} if none of this element is found.
}
\author{
Regis Pouillot
}
\note{
This function does not test if the object is correct. See
\code{\link{is.mcnode}}.
}
\keyword{utilities}
