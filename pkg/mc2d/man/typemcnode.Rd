\name{typemcnode}
\alias{typemcnode}
\title{
Provides the Type of a mcnode Object
}
\description{
Provide the type of a mcnode object.
}
\usage{
typemcnode(x, index=FALSE)
}
\examples{
data(total)
typemcnode(total$xVUM2)
}
\arguments{
\item{x}{a mcnode object}
\item{index}{if TRUE give the index of the type rather than the
type.}
}
\value{
"0", "V","U" or "VU" or the corresponding index if index==TRUE.

 NULL if none of this element is found.
}
\author{
Regis Pouillot
}
\note{
This function does not test if the object is correct. See
\code{\link{is.mcnode}}.
}
\keyword{utilities}
