\name{list2chain}
\alias{list2chain}
\title{
transforms a consistent list into a new chain object
}
\description{
Just analyzing the components of the list (consistent names have to
be used) which are supposed to be character and tackle them to
produce consistent slots of a /des/ object and /nod/ objects. The
main use of this function is to generate chains read from text files
with the function \code{read8chain}.
}
\usage{
list2chain(li, chaind=1, nod=bf(li)[-1])
}
\examples{
 prr3k("RESET"); # (only for R checking)
 print(list2chain(chain2list(prr.chain1)));
}
\arguments{
\item{li}{The list to be transformed into a chain object. The
chaind-th component must be the description of the bn; the nod
components are supposed to be the nodes.}
\item{chaind}{ the number of the \code{li} component to be
interpreted as the description of the chain.}
\item{nod}{ the numbers of the \code{li} to be considered as defining
a node (in the right order).}
}
\details{
Be aware that the node must be given in a compatible order with the
sequential construction of the chain, that is parents before
children.
}
\value{
The generated /chain/ object
}
