\name{draw8sele}
\alias{draw8sele}
\title{
returns the candidate individuals for an empidata node.
}
\description{
(dn) For a given individual (\code{ind}), the distances with all
individuals in the rows of \code{E} are computed according to some
requirement. From them a set of candidates of the \code{E} rows is
returned. This set is comprised of candidates with distances greater
or equal than di[1] and less or equal than di[2]. Their number is at
least equal to nb[1] and no more than nb[2]. When the number of
candidates is too small No candidates are returned; when it is too
big the maximum number is returned with the smallest distances. \cr
Details about the distance formula are given into the /rebastaba/
manual.
}
\usage{
draw8sele(ind, E, wgt, kk, di, nb, nat)
}
\examples{
 rebastaba3k("RESET"); # to comply R checking
 E <- data.frame(A=1:100, c=100:1); # the empirical distribution
 draw8sele(data.frame(A=20), E, structure(1, .Names="A"), 1, c(0, 1), c(1, 2), 
 structure("conti", .Names="A"));
}
\arguments{
\item{ind}{Value of the individual to predict (one row data.frame.}
\item{E}{data.frame with the candidates in its rows.}
\item{wgt}{ the weights to be used for the distance computation.}
\item{kk}{ power coefficient for the distance computation.}
\item{di}{ extreme values of acceptable distances.}
\item{nb}{ minimum and maximum numbers of candidates.}
\item{nat}{nature of the \code{ind} variables (named
\code{character}).}
}
\details{
No check are made
}
\value{
a data.frame with the same columns as those of 'E' containing the
selected subset of candidates sorted according to the distance.
Except for those with names given by \code{rebastaba.ena[2]} which
are filled accordingly to the draw (their own distances). \cr When
empty the returned data.frame has got no row.\cr
}
