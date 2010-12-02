\name{pam2gn}
\alias{pam2gn}
\title{
from a pam object generates a gn object
}
\description{
From a pam object (parentship matrix) generates a gn object. Standard
names can be provided for the nodes.
}
\usage{
pam2gn(pam, description=rbsb.des0, nom=rbsb.cha0)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 pam2gn(g4n.pam1);
}
\arguments{
\item{pam}{The pam obect}
\item{description}{(=rbsb.des0) description for the future gn}
\item{nom}{(=rbsb.cha0) Names for the nodes}
}
\value{
a gn object
}
