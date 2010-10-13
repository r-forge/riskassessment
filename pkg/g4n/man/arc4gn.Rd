\name{arc4gn}
\alias{arc4gn}
\title{
adds or remove some arc(s) to or from a gn object
}
\description{
Adds [removes] zero, one arc or a series of arcs to [from] a gn and
returns the gn so modified. When arcs to add already exist, nothing
is done. When arc to remove does not exist, nothing is done.
}
\usage{
arc4gn(gn, from, to, add=TRUE)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 gn1 <- g4n.gn7;
 gn2 <- arc4gn(gn1, c("A", "C"), c("E", "E"));
 gg1 <- arc4gn(gn2, "A", "B", FALSE);
}
\arguments{
\item{gn}{The gn object}
\item{from}{vector of ion describing the starting nodes of the arcs
to add or remove. }
\item{to}{vector of ion describing the ending nodes of the arcs to
add or remove. }
\item{add}{(=TRUE) Adding? (or removing).}
}
\value{
The modified gn object.
}
