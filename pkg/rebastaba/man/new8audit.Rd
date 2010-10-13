\name{new8audit}
\alias{new8audit}
\title{
produced a bn of kind 'audit' from a /gn/
}
\description{
(bn) From a given gn, this function generates a /bn/ adapted for the
specific modeling associated to 'audit'. More precisely:\cr (*) all
nodes are integer.\cr (*) non root nodes are given as value the sum
of the values of their own parents by means of Dirac links.\cr (*)
root nodes are either numcat or the child of specific Dirichlet new
nodes according to the arguments 'proba' and 'scores'.
}
\usage{
new8audit(gn, proba=rbsb.lis0, scores=rbsb.lis0, def.len=5)
}
\arguments{
\item{gn}{The gn to be transformed into bn.}
\item{proba}{ Named list. When a root name is found, the associated
component must be a numeric vector. When they are positive, a numcat
with these values for probas is generated. When at least one of them
is negative, a Dirichlet with these absolute values is generated. It
serves as the parent of the corresponding multinomial node. The
lengths must be consistent with possible scores.\cr When no name can
be found for a root node a uniform numcat is issued.}
\item{scores}{ Named list. When a root name is found, the associated
component must be a vector, the values of wich will be used as
scores. When no name can be found for a root node, standard scores
1,2,... are used. When a root node have no proba neither scores, a
length of def.len is attributed.}
\item{def.len}{ See 'scores'.}
}
\details{
More precisely, root nodes are transformed, and also their names are
modified. Let 'A' and 'B' such root node in /gn/. It is possible that
'A' be transformed into \{'A.multi','A'\} and 'B' into
\{'B.Diric','B.multi','B'\}. 'Diri' means Dirichlet, 'multi' means
multinomial, in fact it is a 'numcat' node and original root node
bears the score.
}
\value{
The generated /bn/ object
}
