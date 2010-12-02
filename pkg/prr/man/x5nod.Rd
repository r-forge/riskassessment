\name{x5nod}
\alias{x5nod}
\title{
computes the x values for a node
}
\description{
x values for the node \code{nod} are returned to prepare a simulation
of size \code{nrow(X)}. Possible parents are extracted from the data
frame \code{X}, these must exist.
}
\usage{
x5nod(nod, X)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 x5nod(prr.nod1, as.data.frame(matrix(NA, 15, 0)));
}
\arguments{
\item{nod}{The concerned /nod/ object.}
\item{X}{Data frame of the previous simulated nodes in columns. When
\code{nod} is an ancestor node (= no parents), it must be provided
for the number of rows.}
}
\details{
No checks about the existence of the parent are made in this very
basic function.
}
\value{
a vector of numeric values, the length of which is the row number of
\code{X}.
}
