\name{rdistri}
\alias{rdistri}
\title{
simulates with a /distri/ object
}
\description{
From the necessary numerical values simulates independent draws from
any /distri/ distribution.
}
\usage{
rdistri(distri, nbr=1)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 rdistri(prr.distri1);
 rdistri(prr.distri2);
 rdistri(prr.distri3);
 rdistri(prr.distri4);
}
\arguments{
\item{distri}{The /distri/ object to simulate with.}
\item{nbr}{Number of repetition for the simulation.}
}
\value{
a vector of the simulated values
}
