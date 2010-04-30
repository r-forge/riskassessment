\name{list2gn}
\alias{list2gn}
\title{
(gn) transforms a consistent list into a new /gn/ object
}
\description{
Just analyzing the components of the list (consistent names have to
be used) which are supposed to be character and tackle them to
produce consistent slots of a ds object and description of the
nodes... The main use of this function is to generate /gn/ read from
text files with the function \code{file2list}.\cr Be aware that the
gn item are nodes, not variables (i.e. all nodes are univariate with
empty variable names.
}
\usage{
list2gn(li, gnd=1, nod=sjl(li)[-1])
}
\arguments{
\item{li}{The list to be transformed into a gn object. The gnd-th
component must be the description of the gn; the nod components are
supposed to be the nodes when the remaining arguments are left to
default values.}
\item{gnd}{(=1) the number of the li component to be interpreted as
the description of the gn.}
\item{nod}{(=sjl(li)[-1] the numbers of the li to be considered as
defining a node.}
}
\details{
The order of the node is irrelevant, and cycles are possible.\cr For
details about the field see the code or 'rebastaba.etg?.dat'
examples.
}
\value{
The generated /gn/ object
}
