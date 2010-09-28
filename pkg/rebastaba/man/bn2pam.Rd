\name{bn2pam}
\alias{bn2pam}
\title{
gets the pam from a /bn/.
}
\description{
(bn) produces the pam object associated to the parentship of "bn"
}
\usage{
bn2pam(bn, nv="n")
}
\examples{
 rebastaba3k("RESET"); # needed when R is checking the examples
 bn2pam(rebastaba.bn3, "v");
 bn2pam(rebastaba.bn3, "n");
}
\arguments{
\item{bn}{bn object.}
\item{nv}{ gives the level at which to proceed, 'n' for node level;
'v' for variable level.}
}
\value{
the dimnamed matrix of parentships
}
