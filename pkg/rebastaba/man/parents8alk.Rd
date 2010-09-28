\name{parents8alk}
\alias{parents8alk}
\title{
finds the parents at the variable level of an /alk/
}
\description{
(ba) finds the variable parents of \code{alk} with respect to
\code{nom}.
}
\usage{
parents8alk(alk, nom, of="v")
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 parents8alk(rebastaba.alk2, rbsb.nom2);
 parents8alk(rebastaba.alk2, rbsb.nom2, of="n");
 parents8alk(rebastaba.alk3, rbsb.nom3);
}
\arguments{
\item{alk}{alk object}
\item{nom}{/nom/ of the supposed /bn/ where the alk will be
inserted.}
\item{of}{indicates at wich level the parents must be described: 'v'
at the variable level and 'n' at the node level.}
}
\details{
The finding of the parents is done by analyzing the different fields
of the /alk/. This is a basic function, no check is performed.
}
\value{
the names of the /alk/ parents alk at the variable or node level.
}
\keyword{misc}
