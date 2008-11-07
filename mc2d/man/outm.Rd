\name{outm}
\alias{outm}
\title{
Output of Nodes
}
\description{
Changes the output of Nodes
}
\usage{
outm(x, value="each", which.node=1)
}
\examples{
data(total)
total$xVUM2
## since outm = NULL
summary(total$xVUM2) 
x <- outm(total$xVUM2, c("min"))
summary(x)
}
\arguments{
\item{x}{A \code{mcnode} or a \code{mc} object.}
\item{value}{The output of the \code{mcnode} for multivariates nodes.
May be "each" (default) if output should be provided for each
variates considered independently, "none" for no output or a vector
of name of function(s) (as a character string) that will be applied
on the variates dimension before any output (ex: "mean", "median",
c("min",max)). The function should have no other arguments and send
one value per vector of values (ex. do not use \code{"range"}).}
\item{which.node}{which node should be changed in a \code{mc} object}
}
\value{
\code{x} with changed \code{outm} attribute.
}
\keyword{methods}
