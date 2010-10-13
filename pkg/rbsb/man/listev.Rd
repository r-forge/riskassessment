\name{listev}
\alias{listev}
\title{
returns the ith element of a list supposed to be numeric
}
\description{
Returns the ith element of a list supposed to be numeric. As
\code{liste[[ith]]} but returns \code{numeric(0)} if the length of
the list is shorter than \code{ith}.
}
\usage{
listev(liste, ith)
}
\examples{
 listev(list(A=123, B=234, C=345), 2);
}
\arguments{
\item{liste}{the list}
\item{ith}{number of the element to return}
}
\value{
The \code{ith} element of liste if it exists \code{numeric(0)}
otherwise.
}
\keyword{utilities}
