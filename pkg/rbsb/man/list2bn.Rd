\name{list2bn}
\alias{list2bn}
\title{
(bn) transforms a consistent list into a new bn object
}
\description{
Just analyzing the components of the list (consistent names have to
be used) which are supposed to be character and tackle them to
produce consistent slots of a des object and alk objects. The main
use of this function is to generate bn read from text files with the
function \code{file2list}.
}
\usage{
list2bn(li, bnd=1, nod=sjl(li)[-1])
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(list2bn(bn2list(rbsb.bn4)));
}
\arguments{
\item{li}{The list to be transformed into a bn object. The bnd-th
component must be the description of the bn; the nod components are
supposed to be the nodes when the remaining arguments are left to
default values.}
\item{bnd}{(=1) the number of the li component to be interpreted as
the description of the bn.}
\item{nod}{(=sjl(li)[-1] the numbers of the li to be considered as
defining a node.}
}
\details{
Be aware that the node must be given in a compatible order with the
sequential construction of the bn
}
\value{
The generated 'bn' object
}
