\name{nbnv}
\alias{nbnv}
\title{
number of nodes/variables for a /nom/ or an /object@nom/
}
\description{
According to 'what', returns the number of nodes/variables for a
\code{nom} or an object comprising a slot \code{@nom}.
}
\usage{
nbnv(x, what=-1)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 nbnv(rbsb.nom2); # number of nodes
 nbnv(rbsb.nom2, 0); # number of variates 
}
\arguments{
\item{x}{the bn/gn/dn/gn or nom object}
\item{what}{ -1: returns the number of nodes 0: returns the total
number of variables i: returns the number of variables of the i-th
node. (For convenience 'n' is translated as -1 and 'v' is translated
as 0).}
}
\details{
no check is performed.
}
\value{
The number of nodes or variables, accordingly to what
}
\keyword{misc}
