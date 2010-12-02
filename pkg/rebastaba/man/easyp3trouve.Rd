\name{easyp3trouve}
\alias{easyp3trouve}
\title{
checks the consistency of the result obtained after easyp3cut and
returns the used node/variable numbers of nom
}
\description{
(ba) The aim of this function is to precise the analysis made by
easyp3cut applying it to a precise nom object. Using the composition
of easyp3trouve(easyp3cut(chain),nom) gives a result directly usable
(for well introduced users of rebastaba). The check is made on the
validity of node names, on the validity of the numerical constants.
Completed information about the \$typ=2 (nodes and variables) is
returned.
}
\usage{
easyp3trouve(x, nom)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 uu <- easyp3cut("sqrt({{A}}) + {{U}}/10", rbsb.cpt);
 easyp3trouve(uu, char2nom(LETTERS));
}
\arguments{
\item{x}{output of easyp3cut}
\item{nom}{nom object where node/variable names will be investigated}
}
\details{
when a node is involved, all its variables are considered as
involved. If A is a node repeated three times, then "A" implies
"A[1]", "A[2]" and "A[3]" as variables but "A[2]" implies node "A"
and only variable "A[2]".
}
\value{
A list comprising \$node: the internal number (i with respect to nom)
of the nodes involved,\cr \$vari: the internal number (k with respect
to nom) of the variables involved,\cr
}
\keyword{utilities}
