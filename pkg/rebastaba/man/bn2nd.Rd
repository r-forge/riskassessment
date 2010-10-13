\name{bn2nd}
\alias{bn2nd}
\title{
extracts and returns some traits of a node defined into a bn
}
\description{
(bn) The extraction is made for only one node but several different
extractions can be asked when 'what' is a vector.\cr The hope is that
function be used very extensively to ease further modifications
within the /bn/ object structure.
}
\usage{
bn2nd(bn, nod, what, check=TRUE)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 bn2nd(rebastaba.bn2, "B", "par");
 bn2nd(rebastaba.bn2, "B", "asc");
 bn2nd(rebastaba.bn2, "B", "chi");
 bn2nd(rebastaba.bn2, "B", "des");
 bn2nd(rebastaba.bn2, "B", "nat");
}
\arguments{
\item{bn}{The /bn/ from which the extraction will be made.}
\item{nod}{The node (name or nk specification) to be extracted.}
\item{what}{A character defining by each of its composents what must
be extracted. The different possibilities are:\cr 'par': the direct
parents of the node (in name specification)\cr 'asc': all ascendants
of the node (in name specification)\cr 'chi': the direct parents of
the node (in name specification)\cr 'des': all ascendants of the node
(in name specification)\cr 'nat': variable natures o}
\item{check}{Must checking be made?}
}
\details{
The characteristics which are at the bn@nom level (like the variable
names, indices of the variable node) are not included here because
they might exist for this object.
}
\value{
A list with as many components as asked by 'what's with the same
names. Each component being the computed response built up on the
/bn/ exploration.
}
