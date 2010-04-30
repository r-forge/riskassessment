\name{cycle8pam}
\alias{cycle8pam}
\title{
(ba) detects if a cycle exists from a pam matrix
}
\description{
This function returns TRUE or FALSE if at least one cycle is detected
within the graph defined by the pam matrix.
}
\usage{
cycle8pam(pam)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 papa <- new("pam", rlt=matrix(c(0, 0, 1, 1, 0, 0, 0, 1, 0), 3));
 cycle8pam(papa);
}
\arguments{
\item{pam}{The pam matrix}
}
\value{
logical variable (TRUE: at least a cycle, FALSE : no cycle).
}
\keyword{utilities}
