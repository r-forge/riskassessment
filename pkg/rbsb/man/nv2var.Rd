\name{nv2var}
\alias{nv2var}
\title{
returns the variable name from a node[variable] name
}
\description{
Just removing the node name and square brackets. \code{nvn} can be a
vector. In case there is no variable name, the standard "" is
returned
}
\usage{
nv2var(nvn)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 nv2var(c("A[5]", "B"));
}
\arguments{
\item{nvn}{ character of the complete variable name}
}
\details{
Variable name can be numeric and is returned as such
}
\value{
variable name without node name
}
\keyword{misc}
