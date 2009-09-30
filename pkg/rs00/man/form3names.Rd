\name{form3names}
\alias{form3names}
\title{
(f0) provides systematic names for nodes
}
\description{
Provides systematic names for nodes according the number of nodes
taking care of not duplicating former nodes.
}
\usage{
form3names(nbn, nom=character(0), prefix="")
}
\examples{
 form3names(2);
}
\arguments{
\item{nbn}{Number of new node names to generate}
\item{nom}{\}) Already present names (to avoid identical names).}
\item{prefix}{ Systematic prefix of all names to generate. Must
comprise the dot, if one wants suh a separator between it and the
specific part of the name. Of course can be underscore or whatever
else.}
}
\details{
(see the code)
}
\value{
vector with nbn different strings associated to new names
}
\keyword{utilities}
