\name{rnorbn}
\alias{rnorbn}
\title{
creates a pseudo-random normal bn object from a given bn
}
\description{
(bn) From a gn object adds on all nodes normal probability
distributions. The parameters for these are pseudo-randomly choosen
according to arguments creg and erre.\cr Using this function with the
same set.seed starting point (and the same request) must produce
identical bn.\cr The gn can be any graph (without cycle) for instance
designed by hand or resulting from rgn.
}
\usage{
rnorbn(gn, description=new("des", name=gn@description@name), creg=c(0, 5),
	  erre=c(0, 1), ndec=1)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print(rnorbn(g4n.gn7));
}
\arguments{
\item{gn}{The starting gn object.}
\item{description}{ The description for the created object (des
object). It can be a simple name.}
\item{creg}{ The mu and sigma parameters to draw the coefficient of
regression (associated to the mean and the possible parents) with
rnorm.}
\item{erre}{ The two limits to draw the standard deviation of the
error with runif.}
\item{ndec}{ rounding argument for the parameter values.}
}
\details{
Have a look to the code, not too difficult to follow. ??? Add some
details with explained examples
}
\value{
a bn object comprising only normal variables
}
