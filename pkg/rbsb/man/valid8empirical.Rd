\name{valid8empirical}
\alias{valid8empirical}
\title{
checks an /empirical/
}
\description{
This function checks an /empirical/ object
}
\usage{
valid8empirical(object)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 valid8empirical(rbsb.epi1);
}
\arguments{
\item{object}{The empirical object to be validated.}
}
\details{
It is the validity method for /empirical/ objects.
}
\value{
TRUE when the object seems acceptable else a character describing the
error(s)
}
\keyword{classes}
