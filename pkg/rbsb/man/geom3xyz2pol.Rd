\name{geom3xyz2pol}
\alias{geom3xyz2pol}
\title{
transforms cartesian coordinates in polar coordinates
}
\description{
Transforms, into R to three, cartesian coordinates in polar
coordinates
}
\usage{
geom3xyz2pol(coo)
}
\examples{
 geom3xyz2pol(c(1/sqrt(2), 1/sqrt(2), 0));
}
\arguments{
\item{coo}{the x,y,z coordinates.}
}
\value{
A numerical vector with (rho, theta, phi)
}
\keyword{utilities}
