\name{draw8predg}
\alias{draw8predg}
\title{
returns the predicted individuals for an empidata node.
}
\description{
(dn) returns the predicted individuals for an empidata node when they
are drawn globally. The description of the algorithm is given into
the /rebastaba/ manual. The prediction is monitored by the /win/.
}
\usage{
draw8predg(nbs, EE, win)
}
\examples{
 rebastaba3k("RESET"); # to comply R checking
 EE <- data.frame(A=1:100, c=100:1); # the empirical distribution
 win <- new("win", nat=structure("conti", .Names="c"), swg=structure(1, .Names="A"), skk=0, sdi=c(0, 1), snb=c(1, 20));
 draw8predg(20, EE, win);
 win2 <- new("win", rty=c("*", "systematic"));
 draw8predg(10, EE, win2);
}
\arguments{
\item{nbs}{number of individuals to predict.}
\item{EE}{data.frame with the candidates in its rows.}
\item{win}{ the /win/ to be used for the selection process.}
}
\value{
a data.frame of \code{nbs} rows and all columns of \code{EE}.
}
