\name{form3nd}
\alias{form3nd}
\title{
(bn) symbolizes the kind of nodes
}
\description{
A character vector is returned giving for each node designated in
'kn', the number of parents and number of children.
}
\usage{
form3nd(bn, kn=sj(nbnv(bn)))
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 form3nd(rbsb.bn1, 1);
 form3nd(rbsb.bn1);
}
\arguments{
\item{bn}{The original bn (to get the genealogy).}
\item{kn}{(=sj(nbnv(bn))) indicates the nodes to specify (numeric
vector)}
}
\details{
Description of the symbols:\cr " (\#) " : isolated node\cr "
-\eqn{>}(\#) " : leaf node with one parent\cr "3=\eqn{>}(\#) " : leaf
node with three parents\cr " -\eqn{>}(\#)-\eqn{>} " : transmitter
node\cr "2=\eqn{>}(\#)=\eqn{>}2" : two-parents two-children node\cr
where '\#' stands for the node name.
}
\value{
The resulting named vector of symbols
}
