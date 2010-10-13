\name{bn2list}
\alias{bn2list}
\title{
transforms a /bn/ into a list
}
\description{
(bn) Just translating all components of a /bn/ into a list compatible
with \code{list2bn}.
}
\usage{
bn2list(bn)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 bn2list(rebastaba.bn4);
 bn2list(rebastaba.bn6);
}
\arguments{
\item{bn}{The /bn/ to be transformed.}
}
\value{
The generated list
}
