\name{rr8nod}
\alias{rr8nod}
\title{
computes the central value of a node
}
\description{
From the named vector \code{x} where the possible parents must exist,
the deterministic application of the central value is done for the
node \code{nod}.
}
\usage{
rr8nod(nod, x=NULL, add=TRUE)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 x1 <- rr8nod(prr.nod1);
 x2 <- rr8nod(prr.nod2, x1);
 x3 <- rr8nod(prr.nod3, x2);
}
\arguments{
\item{nod}{/nod/ object to be simulated.}
\item{x}{named vector of the previous computed nodes, can be anything
for the ancestor nodes.}
\item{add}{Must the vector \code{x} be augmented, or only the the new
value returned?}
}
\value{
Named vector with the computed and retained node possibly as an
augmentation of the inputted \code{x}.
}
