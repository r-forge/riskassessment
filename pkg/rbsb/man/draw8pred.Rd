\name{draw8pred}
\alias{draw8pred}
\title{
(dn) returns the predicted individual for an empidata node.
}
\description{
The description of the algorithm is given into /rbsb/ manual. The
prediction is monitored by the /win/.
}
\usage{
draw8pred(ind, E, nat, win=new("win", wgt=rep(1, length(nat)), k=2, di=c(0,
	  1), nb=c(1, Inf), ty="media1"))
}
\examples{
 rsdn3k("RESET"); # to comply R checking
 E <- data.frame(A=1:100, c=100:1); # the empirical distribution
 win <- new("win", wgt=1, k=0, di=c(0, 1), nb=c(1, 2));
 draw8pred(data.frame(A=20), E, c("conti", "conti"), win);
}
\arguments{
\item{ind}{Value of the individual to predict (one row data.frame.}
\item{E}{data.frame with the candidates in its rows.}
\item{nat}{nature of the E variables (character).}
\item{win}{(=new("win",wgt=rep(1,length(nat)),
k=2,di=1,nb=c(1,Inf),ty="median1")), the /win/ to be used for the
selection process.}
}
\details{
When the global variable rbsb.esu is true then the selected subset is
appended to rbsb.efi
}
\value{
a list with components:\cr \$pred: the predicted individual (with all
E-variables but with condi values forced to those of ind).\cr
\$nb\_c: size of the sub-sample from which it was produced.\cr
\$di\_x: the maximum distance of the considered candidates
}
