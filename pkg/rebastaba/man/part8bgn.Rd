\name{part8bgn}
\alias{part8bgn}
\title{
classifies a set of nodes according to a subset of them
}
\description{
(bn) From a bn/gn structure and a subset of nodes, the set of nodes
is partitioned into four exclusive classes:\cr (i) the targetted
subset ["nod"],\cr (ii) their direct parents (excluding all of them)
["par"/"chi"],\cr (iii) their other ascendance ["asc"/"des"],\cr (iv)
the remaining nodes ["nas"/"nde"].\cr Of course, some of the classes
can be empty. The partition can be established for the descendance.
}
\usage{
part8bgn(bgn, ion, asc=TRUE)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print(part8bgn(rebastaba.bn2, "A"));
 print(part8bgn(rebastaba.bn2, "B"));
 print(part8bgn(rebastaba.bn1, "C"));
}
\arguments{
\item{bgn}{The original bn or gn.}
\item{ion}{The definition of targetted subset of nodes (iin or name
vector). It is checked that it is a subset.}
\item{asc}{ Must the partition be done for ascendance (TRUE) or
descendance (FALSE).}
}
\details{
No check is done about \code{bgn} validity
}
\value{
a list of named numeric vectors with the names of the corresponding
nodes. The list names are given in the description field between
squared brackets.
}
