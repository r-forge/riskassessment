\name{print8link}
\alias{print8link}
\title{
(bn) prints an interpretation of one link of a bn
}
\description{
This function prints how rebastaba will interpret the link as defined
for a precised node (nk) of a /bn/.
}
\usage{
print8link(bn, nk, quoi="dl")
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 print8link(rbsb.bn1, 2);
}
\arguments{
\item{bn}{The bn object.}
\item{nk}{Identification number of the node.}
\item{quoi}{(="dl") d for description, l for link.}
}
\details{
The interpretation is hand made for each type of links. The node name
is not given because it could be more convenient to print it outside
this function with some adequate format (as done by print8links).
Global constant rbsb.cond and rbsb.emnd are used for the printing of
the description of the nodes.
}
\value{
nothing but a print is performed
}
