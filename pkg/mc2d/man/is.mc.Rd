\name{is.mc}
\alias{is.mc}
\alias{is.mcnode}
\title{
Tests mc and mcnode Objects
}
\description{
is.mc tests mc objects and is.mcnode tests mcnode objects.
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
\item{x}{An mc or a mcnode object.}
}
\details{
is.mc tests if x is a list of mcnode, each elements being of
compatible dimension. It tests if the class "mc" is affected to the
object.

 is.mcnode tests if x is an array of numeric or logical, if it has a
"type" attribute and compatible dimensions, and if the class "mcnode"
is affected to the object.
}
\value{
TRUE or FALSE
}
\author{
Regis Pouillot
}
\keyword{utilities}
