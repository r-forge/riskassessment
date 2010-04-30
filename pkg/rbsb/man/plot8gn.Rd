\name{plot8gn}
\alias{plot8gn}
\title{
(gn) plots an object gn
}
\description{
Standard plot of an object gn.\cr Following some coding found in
slots x@pos and x@arc, decorations are added to the arcs and to the
nodes. More precisely:\cr Each row of the matrix x@pos@posi is
associated to a node. The first three columns give the (X,Y,Z)
coordinates (in /rbsb/ graphs are placed in R to the 3 and the 4th
column indicates if the node must be surrounded by nothing (0), a
square (1), a circle (2).\cr x@arc@fle is a matrix with rows
associated to arcs. The first column is the iin of the departure node
and the second the iin of the arrival node. The 3rd column indicates
the style of the arrow by three combined parameters com =
100*code+10*lty+lwd. code is the standard parameter of basic R arrows
function, lty and lwd parameters are standard ones from par... The
standard value of com is 211.
}
\usage{
plot8gn(x, y="useless", ..., main=NULL, frame=FALSE)
}
\examples{
 rsgn3k("RESET"); # needed only for R checking, to be forgotten
 plot8gn(rbsb.gn1);
}
\arguments{
\item{x}{the gn to plot}
\item{y}{added useless argument to comply R requirement.}
\item{\dots}{Further arguments to be passed to the plot function.}
\item{main}{(=NULL) main title for the graph, when NULL
x@description@name is taken as title.}
\item{frame}{(=FALSE) to indicate if a frame has to be drawn}
}
\details{
Possible decorations for nodes are: square (1), circle (2).\cr
x@arc@fle[,3] determines the style of the arcs: cdu\cr c gives the
kind of arrows (see R function arrow): \{0,1,2,3\}\cr d gives the
type of lines: \{0,1,...,6\}\cr u gives the width of the line:
[0.5,5]
}
\value{
returns nothing but a plot is drawn
}
