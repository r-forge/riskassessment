\name{parents5nod}
\alias{parents5nod}
\title{
extracts the parents of a /nod/
}
\description{
returns the parents (from the \code{nod@x} slot) of a given node.
}
\usage{
parents5nod(nod)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 parents5nod(prr.nod2);
}
\arguments{
\item{nod}{The /nod/ to be considered.}
}
\value{
A character with the name of the parents. When there are no parents:
\code{character(0)}.
}
