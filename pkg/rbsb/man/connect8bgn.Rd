\name{connect8bgn}
\alias{connect8bgn}
\title{
(bn) determines if a bn/gn is connected
}
\description{
exploring the links between nodes returns the sub-sets of connected
nodes.
}
\usage{
connect8bgn(bgn)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 cat("To be added\n");
}
\arguments{
\item{bgn}{the bn or gn object to investigate}
}
\value{
A vector associated to the nodes indicating subsets. If res[3] equals
2 this means that according to the finding, the third iin node
belongs to the second subset. The number of subsets is then max(res).
The numbering is done in decreasing order of size of the subsets.
Names of the res vector are names of the nodes.
}
\keyword{utilities}
