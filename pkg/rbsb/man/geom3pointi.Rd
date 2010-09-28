\name{geom3pointi}
\alias{geom3pointi}
\title{
interpolation in R to the 2
}
\description{
Interpolation in the plane: given two points p0=(x0,y0) and
p1=(x1,y1) returns the point p=(x,y) such that vector(p0,p) =
z*vector(p0,p1) where z is such that the distance between p0 and p is
abs(d). Also sign(d) == sign(z)
}
\usage{
geom3pointi(x0, y0, x1, y1, d)
}
\examples{
 geom3pointi(1, 1, 10, 10, sqrt(2));
}
\arguments{
\item{x0}{see description}
\item{y0}{see description}
\item{x1}{see description}
\item{y1}{see description}
\item{d}{see description}
}
\value{
Returns c(x,y), the interpolated points.
}
\keyword{utilities}
