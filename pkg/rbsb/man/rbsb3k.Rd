\name{rbsb3k}
\alias{rbsb3k}
\title{
assigns the constants for the rbsb layer
}
\description{
defines or returns the constants used within /rbsb00/. The performed
action depends on the argument.
}
\usage{
rbsb3k(whi)
}
\examples{
 rbsb3k("reset");
 print(rbsb.msi);
 rbsb3k("names");
 rbsb3k("values");
}
\arguments{
\item{whi}{a character(1) indicating either to reset or to return the
names or the current values. The three values are \code{RESET},
\code{reset}, \code{names}, \code{definitions} or \code{values}.}
}
\details{
All constant names start with 'rbsb.'. This solution was adopted to
replace a set of global constants that I had difficulty to make
acceptable with R packages standards. It is recommended not to modify
these constants unless you are completely aware of the
consequences.\cr The constants can be any object type.
}
\value{
When \code{whi=="RESET"} nothing (but the assignments are performed
for all layers: only \code{rbsb} since it is the first one.). When
\code{whi=="reset"} nothing (but the assignments of the layer
\code{rbsb} are performed). When \code{whi=="names"} the names as a
character vector. When \code{whi=="definitions"} the definitions as a
named character vector. When \code{whi=="values"} the values through
a named list.
}
\keyword{misc}
