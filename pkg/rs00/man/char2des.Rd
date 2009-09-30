\name{char2des}
\alias{char2des}
\title{
(f1) transforms,if necessary, a single character to a 'des' object
}
\description{
when x is not a character(1) or a 'des' an error is issued
}
\usage{
char2des(x)
}
\examples{
 print(char2des("toto"));
}
\arguments{
\item{x}{name for the created 'des' or an already existing 'des'
ojbect.}
}
\value{
a 'des' object (not modfyied when x was already some one.
}
\keyword{utilities}
