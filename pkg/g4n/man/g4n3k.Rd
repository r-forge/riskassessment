\name{g4n3k}
\alias{g4n3k}
\title{
assigns the constants for the g4n layer
}
\description{
defines or returns the constants used within /rbsbgn/. The performed
action depends on the argument.
}
\usage{
g4n3k(whi)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 print(g4n.msi);
 g4n3k("names");
 g4n3k("values");
}
\arguments{
\item{whi}{a character(1) indicating either to reset or to return the
names or the current values. The possible values are \code{RESET},
\code{reset}, \code{names}, \code{definitions} or \code{values}.}
}
\details{
All constant names start with 'g4n.'. This solution was adopted to
replace a set of global constants that I had difficulty to make
acceptable with R packages standards. It is recommended not to modify
these constants unless you are completely aware of the
consequences.\cr The constants can be any object type.
}
\value{
When \code{whi=="RESET"} nothing (but the assignments are performed
for all layers \code{rbsb} and \code{g4n}). When \code{whi=="reset"}
nothing (but the assignments of the layer \code{g4n} are performed).
When \code{whi=="names"} the names as a character vector. When
\code{whi=="definitions"} the definitions as a named character
vector. When \code{whi=="values"} the values through a named list.
}
\keyword{misc}
