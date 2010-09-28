\name{form3nd}
\alias{form3nd}
\title{
symbolizes the kind of nodes
}
\description{
(bn) A character vector is returned giving for each node designated
in 'kn', the number of parents and number of children.
}
\usage{
form3nd(bn, kn=bc(nbnv(bn)))
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 form3nd(rebastaba.bn1, 1);
 form3nd(rebastaba.bn1);
}
\arguments{
\item{bn}{The original bn (to get the genealogy).}
\item{kn}{ indicates the nodes to specify (numeric vector)}
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
