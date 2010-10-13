\name{cycle8bgn}
\alias{cycle8bgn}
\title{
detects if a cycle exists in a bn/gn object
}
\description{
(bn) This function returns TRUE or FALSE if at least one cycle is
detected within the graph of the bgn object. It is intended to
protect other function which arises fatal errors in such a situation
(for instance porder).\cr
}
\usage{
cycle8bgn(bgn)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 cycle8bgn(rebastaba.bn2);
}
\arguments{
\item{bgn}{The bn or gn object}
}
\details{
No check is performed on the class of \code{bgn}
}
\value{
logical variable (TRUE: at least a cycle, FALSE : no cycle).
}
\keyword{utilities}
