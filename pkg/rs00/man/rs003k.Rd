\name{rs003k}
\alias{rs003k}
\title{
(00) assigns the constants for the rs00 layer
}
\description{
defines or returns the constants used within /rbsb00/. The performed
action depends on the argument.
}
\usage{
rs003k(whi)
}
\examples{
 rs003k("reset");
 print(rbsb.msi);
 rs003k("names");
 rs003k("values");
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
for all layers: only \code{rs00} since it is the first one.). When
\code{whi=="reset"} nothing (but the assignments of the layer
\code{rs00} are performed). When \code{whi=="names"} the names as a
character vector. When \code{whi=="definitions"} the definitions as a
named character vector. When \code{whi=="values"} the values through
a named list.
}
\keyword{misc}
