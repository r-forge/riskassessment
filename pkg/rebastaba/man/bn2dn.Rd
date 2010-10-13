\name{bn2dn}
\alias{bn2dn}
\title{
simulates data from a bn
}
\description{
(dn) This function creates a /dn/ object simulating bn \code{simu}
times.
}
\usage{
bn2dn(bn, simu=100, score=TRUE)
}
\examples{
 rebastaba3k("RESET"); # For R convenience when checking
 print(bn2dn(rebastaba.bn1, 10), quoi="i", simu=100);
 print(bn2dn(rebastaba.bn2, 10), quoi="i", simu=100);
 print(bn2dn(rebastaba.bn3, 10), quoi="i", simu=100);
 print(bn2dn(rebastaba.bn4, 10), quoi="i", simu=100);
 print(bn2dn(rebastaba.bn5, 10), quoi="i", simu=100);
 print(bn2dn(rebastaba.bn6, 10), quoi="i", simu=100);
}
\arguments{
\item{bn}{the bn object with which to simulate}
\item{simu}{ Either (1) a \code{numeric} indicating the desired
number of simulations, or (2) a data.frame whose the number of rows
indicates the desired number of simulations, and where columns can
comprises already computed nodes. More details are given in the
documentation of \code{simulate8bn}.}
\item{score}{ When TRUE, an additional variable scoring the
likelihood of each individual according to the variable domains is
added. See the function \code{dn2score} for more details.}
}
\details{
In a first version, the function was performing a genuine simulation
from the distributions associated to each node of the bn. Then, it
was found convenient to allow 'already known nodes' using simu as a
matrix.
}
\value{
The resulting dn object
}
