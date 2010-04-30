\name{code4bn}
\alias{code4bn}
\title{
(bn) builds the code for the nk-th node of a /bn/.
}
\description{
When bugs=FALSE this function completes the @nfug component for the
nk-th given node. When bugs=TRUE, the result is the bugs code which
has a similar structure.
}
\usage{
code4bn(bn, nk, bugs=FALSE)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 uu <- code4bn(rbsb.bn2, 1);
 print(uu, quoi="g", qui=1);
}
\arguments{
\item{bn}{The bn}
\item{nk}{node ('nk') for which the code must be calculated.}
\item{bugs}{(=FALSE) when TRUE computes bugs code.}
}
\value{
The resulting bn completed or when bugs the bugs code as a character
vector.
}
