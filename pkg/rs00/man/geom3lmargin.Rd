\name{geom3lmargin}
\alias{geom3lmargin}
\title{
(f0) adds some padding to a range
}
\description{
Used for the preparation of graphic outputs: the range of 'x' is
extended of a proportion 'ma' at both ends
}
\usage{
geom3lmargin(x, ma=0.15)
}
\examples{
 geom3lmargin(1:5);
 geom3lmargin(1:5, 0.1);
}
\arguments{
\item{x}{vector of values to consider}
\item{ma}{proportion margin to add}
}
\value{
A vector of two numerical giving the extended range
}
\keyword{utilities}
