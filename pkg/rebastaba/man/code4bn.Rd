\name{code4bn}
\alias{code4bn}
\title{
builds the code for the nk-th node of a /bn/.
}
\description{
(bn) When bugs=FALSE this function completes the @nfug component for
the nk-th given node. When bugs=TRUE, the result is the bugs code
which has a similar structure.
}
\usage{
code4bn(bn, nk, bugs=FALSE)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 uu <- code4bn(rebastaba.bn2, 1);
 print(uu, quoi="g", qui=1);
}
\arguments{
\item{bn}{The bn}
\item{nk}{node ('nk') for which the code must be calculated.}
\item{bugs}{ when TRUE computes bugs code.}
}
\value{
The resulting bn completed or when bugs the bugs code as a character
vector.
}
