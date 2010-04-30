\name{nanv}
\alias{nanv}
\title{
(ba) names of nodes/variables for an bn/gn/dn/nom object
}
\description{
According to 'what', returns the names of nodes/variables for an
bn/gn/dn/nom object
}
\usage{
nanv(x, what=-1)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 nanv(rbsb.nom2) # node names
 nanv(rbsb.nom2, 0) # variable names
}
\arguments{
\item{x}{the bn/gn/dn/gn or nom object}
\item{what}{(="-1") -1: returns the names of the nodes 0: returns all
names of the variables i: returns the names of the variables of the
i-th node. (For convenience 'n' is translated as -1 and 'v' is
translated as 0).}
}
\details{
No check is made about \code{x}
}
\value{
The names of nodes or variables, accordingly to what
}
\keyword{misc}
