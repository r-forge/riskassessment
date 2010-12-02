\name{draw8dist}
\alias{draw8dist}
\title{
returns the distances for the selection to be operated with an
empidata node.
}
\description{
(dn) The description of the algorithm is given into /rebastaba/
manual. The selection is monitored by the weights and power
coefficient usually associated to a /win/.
}
\usage{
draw8dist(ind, E, wgt, kk, nat)
}
\examples{
 rebastaba3k("RESET"); # to comply R checking
 E <- data.frame(A=1:100, c=100:1); # the empirical distribution
 wgt <- structure(1, .Names="A");
 nat <- structure("conti", .Names="A");
 draw8dist(data.frame(A=20), E, wgt, 1, nat);
}
\arguments{
\item{ind}{Value of the individual to predict (one row data.frame.}
\item{E}{data.frame with the candidates in its rows.}
\item{wgt}{ Named weight(s) to be used for the computation of the
distances. When this argument is null, the same distance is returned
for all rows of \code{E}.}
\item{kk}{ Power coefficient to be used for the computation of the
distances.}
\item{nat}{natures of the \code{ind} variables (\code{named
character}).}
}
\details{
No systematic check of the argument at this level
}
\value{
a numeric vector of length \code{nrow(E)} containing the distance of
\code{ind} with each row of \code{E}.
}
