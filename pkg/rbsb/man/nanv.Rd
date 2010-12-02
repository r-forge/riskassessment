\name{nanv}
\alias{nanv}
\title{
returns the names of nodes/variables of x2nom(nom)
}
\description{
According to 'what', returns the names of nodes/variables for
\code{nom} or \code{nom@nom}.
}
\usage{
nanv(x, what=-1)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 nanv(rbsb.nom2) # node names
 nanv(rbsb.nom2, 0) # node[variable] names
 nanv(rbsb.nom2, -2) # variable names
}
\arguments{
\item{x}{the object to consider}
\item{what}{ -2: returns all names of the variables (without the node
name) -1: returns the names of the nodes 0: returns all names of the
variables (including the node name) i: returns the names of the
variables of the i-th node. (For convenience 'n' is translated as -1
and 'v' is translated as 0).}
}
\details{
As it is a very basic function, no check is made about the arguments.
}
\value{
The names of nodes or variables, accordingly to \code{what}.
}
\keyword{misc}
