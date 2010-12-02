\name{chain2list}
\alias{chain2list}
\title{
transforms a /chain/ into a list
}
\description{
transforms a /chain/ into a list compatible with the function
\code{list2chain}. The main use of this function is to be a part of
the function \code{write8chain}.
}
\usage{
chain2list(chain)
}
\examples{
 prr3k("RESET"); # (only for R checking)
 chain2list(prr.chain1);
}
\arguments{
\item{chain}{The chain to be transformed.}
}
\value{
The generated list.
}
