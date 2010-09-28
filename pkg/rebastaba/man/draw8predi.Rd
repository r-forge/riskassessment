\name{draw8predi}
\alias{draw8predi}
\title{
returns the predicted individual for an empidata node.
}
\description{
(dn) returns the predicted individual for an empidata node when they
must be drawn individually. The description of the algorithm is given
into the /rebastaba/ manual. The prediction is monitored by the
/win/.
}
\usage{
draw8predi(ind, EE, win, nat)
}
\examples{
 rebastaba3k("RESET"); # to comply R checking
 EE <- data.frame(A=1:100, c=100:1); # the empirical distribution
 win <- new("win", nat=structure("conti", .Names="c"), swg=structure(1, .Names="A"), skk=0, sdi=c(0, 1), snb=c(1, 20));
 draw8predi(data.frame(A=20), EE, win, structure("conti", .Names="A"));
 win2 <- new("win", nat=structure("conti", .Names="c"), swg=structure(1, .Names="A"), skk=0, 
 sdi=c(0, 10), snb=c(1, 20), rty=c("*", "quantile"), rk2=0.5);
 draw8predi(data.frame(A=20), EE, win2, structure("conti", .Names="A"));
 win3 <- new("win", nat=structure("conti", .Names="c"), swg=structure(1, .Names="A"), skk=0, sdi=c(0, 10), snb=c(1, 20), 
 rty=c("0", "stdev"));
 draw8predi(data.frame(A=20), EE, win3, structure("conti", .Names="A"));
}
\arguments{
\item{ind}{Value of the individual to predict (one row data.frame and
only the variables associated to parents) .}
\item{EE}{data.frame with the candidates in its rows.}
\item{win}{ the /win/ to be used for the selection process.}
\item{nat}{nature of the \code{ind} variables (\code{named
character}).}
}
\details{
Not all the checks are performed
}
\value{
a one-row data.frame returning the predicted individual (with
\code{rebastaba.ena[1]} computed if asked).\cr
}
