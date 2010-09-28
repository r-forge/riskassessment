\name{dim8nd}
\alias{dim8nd}
\title{
computes the dimension of a node
}
\description{
(ba) analyzing the type, parametes and parents of a node returns its
dimension. When it can be computed even when var is not informed, if
this is, it is then checked.
}
\usage{
dim8nd(type, para, parents, nom, var=rbsb.cha0)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 dim8nd("normal", list(mu="{{A}}", sigma="abs({{B}})"), rbsb.cha0, rbsb.nom2);
}
\arguments{
\item{type}{The node type.}
\item{para}{The list of the node parameters.}
\item{parents}{The list of parents (character).}
\item{nom}{The /nom/, to interpret the parent dimensions.}
\item{var}{The provided 'var' list which is compulsory for some node
types.}
}
\value{
The reduced /nom/
}
\keyword{misc}
