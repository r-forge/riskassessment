\name{porder}
\alias{porder}
\title{
(bn) finds a parentship order onto the nodes and more
}
\description{
Proposes an order exploring the parentship of the nodes, such that
parents are always before their children. Such an order can be used
for drawing from the joint distribution.
}
\usage{
porder(bgn)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 porder(rbsb.bn2);
}
\arguments{
\item{bgn}{The bn or gn object}
}
\details{
This assumes no cycle within the graph. If so, the fact is
discovered, a message is issued and the function stops the
program.\cr No check is made about the argument.
}
\value{
order: a compatible order for the calculation of the joint
probability. It is given with respect to the "nk/vk" numbering.
}
\keyword{utilities}
