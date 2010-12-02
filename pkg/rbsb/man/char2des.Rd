\name{char2des}
\alias{char2des}
\title{
transforms, if necessary, a single character into a /des/ object
}
\description{
when x is not a \code{character(1)} or a /des/ an error is issued
}
\usage{
char2des(x)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 print(char2des("toto"));
}
\arguments{
\item{x}{name for the /des/ to be created, or an already existing
/des/.}
}
\value{
a /des/ object (not modfyied when x was already a /des/.
}
\keyword{utilities}
