\name{new8arc}
\alias{new8arc}
\title{
creates an /arc/ with a specified number of nodes and arcs
}
\description{
(ba) After checking that it is possible, returns an /arc/ object with
the specified number of nodes (\code{nbno}) and arcs (\code{nbar}).
}
\usage{
new8arc(nbno, nbar)
}
\examples{
 g4n3k("RESET"); # For R checking to be forgetten
 new8arc(10, 5);
 \dontrun{new8arc(3, 4);}
}
\arguments{
\item{nbno}{Number of desired nodes.}
\item{nbar}{Number of desired arcs.}
}
\value{
A valid \code{arc} object. If not possible, a fatal error is issued
}
\keyword{misc}
