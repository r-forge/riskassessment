\name{print8link}
\alias{print8link}
\title{
prints an interpretation of one link of a bn
}
\description{
(bn) This function prints how rebastaba will interpret the link as
defined for a precised node (nk) of a /bn/.
}
\usage{
print8link(bn, nk, quoi="dl")
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print8link(rebastaba.bn1, 2);
}
\arguments{
\item{bn}{The bn object.}
\item{nk}{Identification number of the node.}
\item{quoi}{ d for description, l for link.}
}
\details{
The interpretation is hand made for each type of links. The node name
is not given because it could be more convenient to print it outside
this function with some adequate format (as done by print8links).
Global constant rebastaba.cond and rebastaba.emnd are used for the
printing of the description of the nodes.
}
\value{
nothing but a print is performed
}
