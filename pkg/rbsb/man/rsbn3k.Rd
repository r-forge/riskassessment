\name{rsbn3k}
\alias{rsbn3k}
\title{
(bn) assigns the constants for the rsbn layer
}
\description{
defines or returns the constants used within /rbsbn/. The performed
action depends on the argument.
}
\usage{
rsbn3k(whi)
}
\examples{
 rsbn3k("RESET");
 print(rbsb.bn2);
 rsbn3k("names");
 rsbn3k("values");
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
for all layers \code{rs00}, \code{rsba}, \code{rsgn} and
\code{rsbn}). When \code{whi=="reset"} nothing (but the assignments
of the layer \code{rsbn} are performed). When \code{whi=="names"} the
names as a character vector. When \code{whi=="definitions"} the
definitions as a named character vector. When \code{whi=="values"}
the values through a named list.
}
\keyword{misc}
