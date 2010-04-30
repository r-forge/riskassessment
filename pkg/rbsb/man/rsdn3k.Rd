\name{rsdn3k}
\alias{rsdn3k}
\title{
(dn) assigns the constants for the rsdn layer
}
\description{
defines or returns the constants used within /rbsdn/. The performed
action depends on the argument.
}
\usage{
rsdn3k(whi)
}
\examples{
 rsdn3k("RESET");
 print(rbsb.dn2);
 rsdn3k("names");
 rsdn3k("values");
}
\arguments{
\item{whi}{(\code{character(1)} indicating either to reset or to
return the names or the current values. The possible values are
\code{RESET}, \code{reset}, \code{names}, \code{definitions} or
\code{values}.}
}
\details{
All constant names start with 'rbsb.'. This solution was adopted to
replace a set of global constants that were difficult to make
acceptable with R packages standards. It is recommended not to modify
these constants unless you are completely aware of the
consequences.\cr The constants can be any object type.
}
\value{
When \code{whi=="RESET"} nothing (but the assignments are performed
for all layers \code{rs00}, \code{rsba}, \code{rsgn} and
\code{rsdn}). When \code{whi=="reset"} nothing (but the assignments
of the layer \code{rsdn} are performed). When \code{whi=="names"} the
names as a character vector. When \code{whi=="definitions"} the
definitions as a named character vector. When \code{whi=="values"}
the values through a named list.
}
\keyword{misc}
