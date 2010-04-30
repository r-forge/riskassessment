\name{listev}
\alias{listev}
\title{
(00) returns the ith element of a list supposed to be numeric
}
\description{
as liste[[ith]] but returns numeric(0) if the length of the list is
shorter than ith
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
The ith element of liste if it exists numeric(0) otherwise
}
\keyword{utilities}
