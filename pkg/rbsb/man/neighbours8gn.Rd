\name{neighbours8gn}
\alias{neighbours8gn}
\title{
(gn) returns the direct parents (or children) of a gn nodes
}
\description{
Returns the parents/children of all nodes (or all variables according
to \code{gn@item}) of 'gn'.
}
\usage{
neighbours8gn(gn, what="parents")
}
\examples{
 rsgn3k("RESET"); # needed only for R checking, to be forgotten
 neighbours8gn(rbsb.gg1, "parents");
 neighbours8gn(rbsb.gg1, "children");
}
\arguments{
\item{gn}{the gn object.}
\item{what}{ either \code{parents} or \code{children} accordingly.}
}
\details{
Parents means direct parents; children means direct children
}
\value{
A list of character vectors containing the names of the parents, each
component of the list is associate to an item (node or variable).
When there is no parent, character(0) is returned.
}
