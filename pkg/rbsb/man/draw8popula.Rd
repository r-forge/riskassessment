\name{draw8popula}
\alias{draw8popula}
\title{
(dn) performs an empidata [multivariate] SYSTEMATIC draw
}
\description{
Returns a systematic draw from E data frame. The returned variables
are defined by ny. Similar to draw8empidata but no parents are
allowed and the draw is systematic. By systematic, it is meant that
the first nbs individuals are used, with cycling if
nrow(E)\eqn{<}nbs.
}
\usage{
draw8popula(ny, E, nbs)
}
\examples{
 rsdn3k("RESET"); # to comply R checking
 E <- data.frame(A=1:10, c=100:91); # the empirical distribution
 draw8popula(c("uu[A]", "vv[c]"), E, 5);
 draw8popula("w[A]", E, 10);
 draw8popula("w[A]", E, 12);
}
\arguments{
\item{ny}{ Name(s) of the variable(s) to simulate (with node name and
square brackets).}
\item{E}{ The data frame to be used for the empidata distribution. It
must comprise indicated in ny BUT without node name and brackets.}
\item{nbs}{ The number of simulations to extract.}
}
\value{
A data frame of size [nbs,length(ny)] with the simulated values in
the right order.
}
