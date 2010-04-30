\name{gn2bn}
\alias{gn2bn}
\title{
(bn) creates a bn object from a gn object
}
\description{
From a gn object, this function creates a bn object. The
transformation is made at the node level.\cr As gn objects are poorer
than bn object, this implies some arbitraryness. Dirac distributions
are imposed everywhere with parameter equals to its nk when no
parents, if not the parent sum.
}
\usage{
gn2bn(gn)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(gn2bn(rbsb.gg1));
}
\arguments{
\item{gn}{the gn object}
}
\value{
the resulting bn object
}
