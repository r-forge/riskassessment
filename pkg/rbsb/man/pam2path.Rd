\name{pam2path}
\alias{pam2path}
\title{
(ba) returns all paths between two nodes
}
\description{
From the parentship matrix computes recursively all paths between two
nodes.
}
\usage{
pam2path(pam, d, a)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 pam2path(rbsb.pam1, 1, 5);
}
\arguments{
\item{pam}{pam object}
\item{d}{departure node (internal number)}
\item{a}{arrival node (internal number)}
}
\details{
The basic idea is quite simple. All paths between d (departure) and a
(arrival) are the paths between every children of d and a, so a
recursive procedure can be used.
}
\value{
a list of vectors describing all possible paths going from d to a.
The list is sorted according to the length of the path.
}
\keyword{utilities}
