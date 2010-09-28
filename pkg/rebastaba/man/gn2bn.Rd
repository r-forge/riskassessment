\name{gn2bn}
\alias{gn2bn}
\title{
creates a bn object from a gn object
}
\description{
(bn) From a gn object, this function creates a bn object. The
transformation is made at the node level.\cr As gn objects are poorer
than bn object, this implies some arbitraryness. Dirac distributions
are imposed everywhere with parameter equals to its nk when no
parents, if not the parent sum.
}
\usage{
gn2bn(gn)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print(gn2bn(g4n.gn7));
}
\arguments{
\item{gn}{the gn object}
}
\value{
the resulting bn object
}
