\name{bn2list}
\alias{bn2list}
\title{
(bn) transforms a /bn/ into a list
}
\description{
Just translating all components of a /bn/ into a list compatible with
\code{list2bn}.
}
\usage{
bn2list(bn)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(bn2list(rbsb.bn4));
}
\arguments{
\item{bn}{The /bn/ to be transformed.}
}
\value{
The generated list
}
