\name{valid8dn}
\alias{valid8dn}
\title{
(dn) checks a /dn/
}
\description{
This function checks /dn/ objects
}
\usage{
valid8dn(object)
}
\examples{
 rsdn3k("RESET"); # for R checking convenience
 valid8dn(rbsb.dn3);
}
\arguments{
\item{object}{The dn object to be validated.}
}
\details{
It is the validity method for /dn/ objects.
}
\value{
TRUE when the object seems acceptable else a character describing the
error(s)
}
\keyword{classes}
