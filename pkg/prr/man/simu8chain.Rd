\name{simu8chain}
\alias{simu8chain}
\title{
simulates values for a given chain
}
\description{
A simulation is done with the object \code{chain}. The number of
values to simulate is given by \code{nbs}. The function returns a
data frame with simulations in rows and variables in columns.
According to the \code{@format} of the nodes, more or less [random]
variables are stored for each node.
}
\usage{
simu8chain(chain, nbs=100)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 simu8chain(prr.chain1, nbs=5);
 simu8chain(prr.chain2, nbs=5);
 simu8chain(prr.chain3, nbs=5);
}
\arguments{
\item{chain}{/chain/ object to be simulated.}
\item{nbs}{An integer indicating the desir}
}
\value{
returns a list with two components 

\item{\$df}{ A data frame with all the simulated (and kept)
variables.}
\item{\$rr}{ A named vector with the results of a deterministic
application of the chain (equivalent to the original Risk Ranger).}
}
