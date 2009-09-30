\name{geom3pol2xyz}
\alias{geom3pol2xyz}
\title{
(f0) transforms polar coordinates in cartesian coordinates
}
\description{
(in R to the three)
}
\usage{
geom3pol2xyz(poo)
}
\examples{
 uu <- geom3xyz2pol(c(1/sqrt(2), 1/sqrt(2), 0));
 geom3pol2xyz(uu);
}
\arguments{
\item{poo}{the polar coordinates (rho, theta, phi)}
}
\value{
A numerical vector with (x, y, z)
}
\keyword{utilities}
