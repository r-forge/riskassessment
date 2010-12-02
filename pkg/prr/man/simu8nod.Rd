\name{simu8nod}
\alias{simu8nod}
\title{
simulates values for a node
}
\description{
From the data frame \code{X} where the possible parents must exist, a
simulation is done for the node \code{nod}. The number of values to
simulate is given by the X row number, so it must be present (even
with zero columns) for ancestor nodes.\cr The function returns an
augmented data frame with a new column (or more according to
\code{nod@format}) in last position(s) containing the simulated
values.
}
\usage{
simu8nod(nod, X=100, add=TRUE)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 X1 <- simu8nod(prr.nod1, 11);
 X2 <- simu8nod(prr.nod2, X1);
 X3 <- simu8nod(prr.nod3, X2);
 simu8nod(prr.nod4, 14);
}
\arguments{
\item{nod}{/nod/ object to be simulated.}
\item{X}{Data frame of the previous simulated nodes in columns. For
ancestor nodes can have zero columns or more simply be an integer
indicating the desired number of simulations, as it is in the
default.}
\item{add}{Must the matrix \code{X} be augmented, or only the the new
columns returned?}
}
\value{
Data frame with the simulated and retained column(s) possibly as an
augmentation of the inputted \code{X}.
}
