\name{nv2nod}
\alias{nv2nod}
\title{
transforms complete variable names into node names
}
\description{
No ckeck is made about the existence of the node names
}
\usage{
nv2nod(vvn)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 nv2nod("aa"); # "aa" is returned
 nv2nod(c("a[e]", "az[ee]", "b")); # c("a", "az", "b") is returned
}
\arguments{
\item{vvn}{vector of variable names}
}
\details{
the syntax analysis is minimal, looking for a square bracket and
eliminating evething from that point !
}
\value{
vector of deduced node names
}
\keyword{misc}
