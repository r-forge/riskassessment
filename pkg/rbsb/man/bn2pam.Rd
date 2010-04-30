\name{bn2pam}
\alias{bn2pam}
\title{
(bn) gets the pam from a /bn/.
}
\description{
produces the pam object associated to the parentship of "bn"
}
\usage{
bn2pam(bn, nv="n")
}
\arguments{
\item{bn}{bn object.}
\item{nv}{(='n') gives the level at which to proceed, 'n' for node
level; 'v' for variable level.}
}
\value{
the dimnamed matrix of parentships
}
