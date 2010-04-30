\name{rsgn3k}
\alias{rsgn3k}
\title{
(gn) assigns the constants for the rsgn layer
}
\description{
defines or returns the constants used within /rbsbgn/. The performed
action depends on the argument.
}
\usage{
rsgn3k(whi)
}
\examples{
 rsgn3k("RESET"); # needed only for R checking, to be forgotten
 print(rbsb.msi);
 rsgn3k("names");
 rsgn3k("values");
}
\arguments{
\item{whi}{a character(1) indicating either to reset or to return the
names or the current values. The possible values are \code{RESET},
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
for all layers \code{rs00}, \code{rsba} and \code{rsgn}). When
\code{whi=="reset"} nothing (but the assignments of the layer
\code{rsgn} are performed). When \code{whi=="names"} the names as a
character vector. When \code{whi=="definitions"} the definitions as a
named character vector. When \code{whi=="values"} the values through
a named list.
}
\keyword{misc}
