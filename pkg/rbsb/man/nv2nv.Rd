\name{nv2nv}
\alias{nv2nv}
\title{
transforms node[variable] characters into node and variable
characters.
}
\description{
(ba) just removing possible square brackets to give back the node
names (\$nod) and the variable names (\$var)
}
\usage{
nv2nv(xx)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 nv2nv(c("A", "A[666]", "")); # list(nod=c("A", "A"), var=c("", "666", ""), vva=c("A", "666", "")
}
\arguments{
\item{xx}{The character to transform}
}
\details{
In fact square brackets are the parentheses given by the constant
\code{rbsb.cpt["variables",]}.
}
\value{
a list with two components\cr 

\item{\$nod}{The node names.}
\item{\$var}{The variable names: '' when absent.}
\item{\$vva}{The variable names but node name when absent.}
}
\keyword{misc}
