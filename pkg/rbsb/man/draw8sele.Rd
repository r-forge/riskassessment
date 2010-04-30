\name{draw8sele}
\alias{draw8sele}
\title{
(dn) returns the selected individuals for an empidata node.
}
\description{
The description of the algorithm is given into /rbsb/ manual. The
selection is monitored by the /win/.
}
\usage{
draw8sele(ind, E, nat, win=new("win", wgt=rep(1, length(nat)), k=2, di=c(0,
	  1), nb=c(1, Inf), ty="median1"))
}
\examples{
 rsdn3k("RESET"); # to comply R checking
 E <- data.frame(A=1:100, c=100:1); # the empirical distribution
 win <- new("win", wgt=1, k=0, di=c(0, 1), nb=c(1, 2));
 draw8sele(data.frame(A=20), E, c("conti", "conti"), win);
}
\arguments{
\item{ind}{Value of the individual to predict (one row data.frame.}
\item{E}{data.frame with the candidates in its rows.}
\item{nat}{nature of the E variables (character).}
\item{win}{(=new("win",wgt=rep(1,length(nat)),
k=2,di=1,nb=c(1,Inf),ty="median1"), the /win/ to be used for the
selection process.}
}
\details{
win@ty slot is not used since only the selection is performed.
}
\value{
a list with components:\cr \$df: a data.frame with the same columns
as those of 'E' containing the selected subset of candidates. This
one can be empty, in that case the returned data.frame has got no
row.\cr \$di\_x: the maximum distance of the subset of returned
candidates
}
