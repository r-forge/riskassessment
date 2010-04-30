\name{draw8dist}
\alias{draw8dist}
\title{
(dn) returns the distances for the selection to be operated with an
empidata node.
}
\description{
The description of the algorithm is given into /rbsb/ manual. The
selection is monitored by the /win/.
}
\usage{
draw8dist(ind, E, nat, win=new("win", wgt=rep(1, length(nat)), k=2, di=c(0,
	  1), nb=c(1, Inf), ty="median1"))
}
\examples{
 rsdn3k("RESET"); # to comply R checking
 E <- data.frame(A=1:100, c=100:1); # the empirical distribution
 win <- new("win", wgt=1, k=0, di=c(0, 1), nb=c(1, 2));
 draw8dist(data.frame(A=20), E, c("conti", "conti"), win);
}
\arguments{
\item{ind}{Value of the individual to predict (one row data.frame.}
\item{E}{data.frame with the candidates in its rows.}
\item{nat}{natures of the E variables (character).}
\item{win}{ the /win/ to be used for the selection process.}
}
\details{
win@ty slot is not used since only the distance values are computed.
}
\value{
a numeric vector of length nrow(E) containing the distance for each
row of E. Of course, it will be \code{numeric(0)}.
}
