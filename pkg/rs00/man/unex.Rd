\name{unex}
\alias{unex}
\title{
(00) collapsing indices
}
\description{
Transforms multiple indices into a unique index following the storage
order of array elements by R.
}
\usage{
unex(xv, di, check=TRUE)
}
\examples{
 rs003k("RESET"); # needed only for R checking, to be forgotten
 uu <- rbsb.dfr1;
 print(cbind(uu, unex(uu, 3:4)));
}
\arguments{
\item{xv}{a N by K matrix, each row corresponding to uple of indices.
each column must be a factor.}
\item{di}{dimensions of the K indices}
\item{check}{(=TRUE) must be checked of the consistency between xv
and di be performed?}
}
\details{
In /rbsb/ this is necessary for dealing with arrays containing
conditional probabilities for categoric variables if one wants to get
paralel drawing of all simulated values with "rmultinom"
}
\value{
A vector of size N with the unique index values
}
\keyword{iteration}
