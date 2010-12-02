\name{ind8bgn}
\alias{ind8bgn}
\title{
returns the nodes between two nodes
}
\description{
(bn) From a bn/gn structure, and two related nodes (first being an
ancestor of second) of it, the set of all nodes having them as
ascendant and descendant (intermediate nodes) is returned. Notice
that the starting and ending nodes are not returned.
}
\usage{
ind8bgn(bgn, ion)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 ind8bgn(rebastaba.bn1, c(1, 3));
 ind8bgn(rebastaba.bn1, c(2, 3));
 \dontrun{ind8bgn(rebastaba.bn2, c(2, 1))}
}
\arguments{
\item{bgn}{The original bn or gn.}
\item{ion}{A pair of nodes (not variables) of bgn.}
}
\details{
No checks about \code{bgn}
}
\value{
The set of concerned nodes without the ion nodes as a named numeric.
So a returned numeric(0) means that ion[1] is a direct parent of
ion[2].
}
