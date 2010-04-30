\name{nparent8alk}
\alias{nparent8alk}
\title{
(ba) finds the parents at the node level
}
\description{
finds the node parents of an alk with respect to \code{nom}.
}
\usage{
nparent8alk(alk, nom)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 nparent8alk(rbsb.alk2, rbsb.nom2);
}
\arguments{
\item{alk}{alk object}
\item{nom}{nom of the bn where the alk must inserted.}
}
\value{
the names of the alk parents composed of nodes and/or variables...
}
