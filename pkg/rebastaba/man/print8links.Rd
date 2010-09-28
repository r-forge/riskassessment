\name{print8links}
\alias{print8links}
\title{
prints the interpretation of some links of a bn
}
\description{
(bn) This function prints how rebastaba will interpret the links of a
bn object.
}
\usage{
print8links(bn, quoi="dl", qui=rbsb.cha0)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print8links(rebastaba.bn1);
}
\arguments{
\item{bn}{The bn object.}
\item{quoi}{ d for description, l for link.}
\item{qui}{ The nodes which must be considered for the printing. The
default implies all nodes, if not either a character vector providing
the nodes or a numeric vector with the iin (internal numbers of
them). The order of printing the nodes is always the natural order
because nv2ion sorts t}
}
\details{
Just a loop over the designated links and printing each
}
\value{
nothing but a print is performed
}
