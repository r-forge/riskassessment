\name{rcatbn}
\alias{rcatbn}
\title{
(bn) creates a pseudo-random numcat bn object
}
\description{
From a gn object adds on all nodes numcat probability distributions.
The parameters for these are pseudo-randomly choosen according to
arguments 'nbcg', 'co' and 're'. For each beta distribution, the two
parameters will be taken equal. This explains why there is only one
interval for each of the two parameters.\cr Using this function with
the same set.seed starting point (and the same request) must produce
identical bn.\cr The gn can be any graph (without cycle) for instance
designed by hand or resulting from rgn.
}
\usage{
rcatbn(gn, description=new("des", name=gn@description@name), nbcg=c(3, 5),
	  co=c(1, 1), re=c(3, 3))
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print(rcatbn(rbsb.gg1), quoi="l", qui=9, proba=TRUE);
}
\arguments{
\item{gn}{The starting gn object.}
\item{description}{(=new("des",name=gn@description@name)) The name
for the created object (description object).}
\item{nbcg}{(=c(3,5)) numbers of categories for each node are chosen
with equal probabilities within the numbers nbcg[1]:nbcg[2].}
\item{co}{(=c(1,1)) The first parameter when calling categ3betap is
drawn with runif with these two values.}
\item{re}{(=c(3,3)) The second parameter when calling categ3betap is
drawn with runif with these two values.}
}
\value{
a bn object comprising only categoric variables
}
