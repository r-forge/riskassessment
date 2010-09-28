\name{plot3D8gn}
\alias{plot3D8gn}
\title{
plots an object gn according to its 3 dimensions
}
\description{
The figure is divided into 4 quadrants, 3 of them are used to draw
the graph according to its (X,Y), (X,Z) and (Z,Y) coordinates
}
\usage{
plot3D8gn(gn, ...)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 plot3D8gn(g4n.gn7);
}
\arguments{
\item{gn}{the gn to plot}
\item{...}{to give when first calling to the standard plot function}
}
\value{
returns nothing but a plot is drawn
}
