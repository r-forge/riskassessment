\name{vparent8alk}
\alias{vparent8alk}
\title{
(ba) finds the parents at the variable level
}
\description{
finds the variable parents of an alk with respect to \code{nom}.
}
\usage{
vparent8alk(alk, nom)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 vparent8alk(rbsb.alk2, rbsb.nom2);
}
\arguments{
\item{alk}{alk object}
\item{nom}{nom of the bn where the alk must inserted.}
}
\value{
the names of the alk composed of nodes and/or variables...
}
