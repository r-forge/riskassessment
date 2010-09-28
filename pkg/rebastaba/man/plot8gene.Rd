\name{plot8gene}
\alias{plot8gene}
\title{
plots ancestry [descendants] of node(s) of a bn/gn
}
\description{
(bn) provides a multi-plot (one plot for each node) showing all
ascendants [or descendants] of a bn or a gn
}
\usage{
plot8gene(bgn, nodes=NULL, what=-1, nbrow=2, nbcol=2)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 \dontrun{plot8gene(g4n.gn7, nbrow=4, nbcol=3);}
}
\arguments{
\item{bgn}{the rbsb object: either a bn or a gn}
\item{nodes}{ vector of ion indicating which nodes to represent, NULL
implies all nodes}
\item{what}{ -1 for ancestors and everythingelse for descendants}
\item{nbrow}{ number of plot rows in a plot}
\item{nbcol}{ number of plot columns in a plot}
}
\details{
if bn@bef [bn@aft] is empty then 'lignee.bn' is called. It is
supposed that they are uptodate. Also if it is of use afterwards,
better to fill it before...\cr No check about \code{bgn}
}
\value{
nothing, a plot is produced
}
