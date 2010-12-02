\name{valid8dn}
\alias{valid8dn}
\title{
checks a /dn/
}
\description{
(dn) This function checks /dn/ objects
}
\usage{
valid8dn(object)
}
\examples{
 rebastaba3k("RESET"); # for R checking convenience
 valid8dn(rebastaba.dn3);
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
