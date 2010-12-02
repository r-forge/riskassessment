\name{prr3k}
\alias{prr3k}
\title{
assigns the prr constants
}
\description{
defines or returns the constants used within /prr/. The performed
action depends on the argument.
}
\usage{
prr3k(whi)
}
\examples{
 prr3k("RESET");
 prr3k("names");
 prr3k("values");
}
\arguments{
\item{whi}{a \code{character(1)} indicating either to reset or to
return the names or the current values. The three possible values are
\code{RESET}, \code{reset}, \code{names}, \code{definitions} or
\code{values}.}
}
\value{
When \code{whi=="RESET"} or \code{whi=="reset"} nothing (but the
assignments of the layer(s) \code{prr} are performed).\cr When
\code{whi=="names"} the names as a character vector are returned.\cr
When \code{whi=="definitions"} the definitions as a named character
vector are returned.\cr When \code{whi=="values"} the values through
a named list are returned.\cr
}
\keyword{misc}
