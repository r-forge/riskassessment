\name{relation4gn}
\alias{relation4gn}
\title{
(gn) formats the arc of a gn from a relation matrix
}
\description{
From a completely described gn and a matrix of scores between all
nodes of it, modifies the format of the arcs to take into account the
strength of their relations.
}
\usage{
relation4gn(gn, rel, cl="equal")
}
\examples{
 rsgn3k("RESET"); # needed only for R checking, to be forgotten
 g0 <- rbsb.gg1; 
 nbn <- nbnv(g0, g0@item); 
 gg <- relation4gn(g0, matrix(1:nbn^2, nbn));
}
\arguments{
\item{gn}{The gn to be modified.}
\item{rel}{The matrix of scores for the relationships. It is a non
symmetric matrix, rows are considered parents of the columns.}
\item{cl}{(='equal') See the details section.}
}
\details{
(1) No arcs are added, only existing arcs are considered.\cr (2) The
set of existing arcs is categorized into four classes : poor
(dashed), small (continuous 1), medium (continuous 2) and strong
(continuous 3).\cr (3) The classes are defined with three limits
defined by cl. cl = 'equal' divides the arcs into approximately four
equally numbered categories. cl = 'propto' divides the range of
categories into four equal scaled categories. cl = numeric(3) imposed
the three limit to use.\cr (4) NA valued arcs by rel are valued as
the "minimum - 1" before determining the categories.
}
\value{
The modified gn
}
