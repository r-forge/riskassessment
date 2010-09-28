\name{plot8path}
\alias{plot8path}
\title{
plots separately all paths between two nodes
}
\description{
(bn) provides a multi-plot (one plot for each path) of all paths
joining the node d to the node a. If there is no path one graph is
performed with no arcs.
}
\usage{
plot8path(bgn, d, a, nbrow=2, nbcol=2, quels=NULL)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 \dontrun{plot8path(g4n.gn7, "A", "F", nbrow=1, nbcol=1);}
 \dontrun{plot8path(g4n.gn7, "A", "B", nbrow=1, nbcol=1);}
}
\arguments{
\item{bgn}{the rbsb object, either a bn or a gn}
\item{d}{departure node}
\item{a}{arrival node}
\item{nbrow}{ number of plot rows in a plot}
\item{nbcol}{ number of plot columns in a plot}
\item{quels}{ which plots to do? NULL means all. If not the number of
the plot to draw, numbering being given after sorting and provided in
the title}
}
\details{
The sub-plots are sorted according to their lengths which are given
in the title as well as their number.\cr No check about \code{bgn}
}
\value{
nothing
}
