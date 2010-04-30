\name{easyps2dim}
\alias{easyps2dim}
\title{
(ba) returns the dimension of the variable defined with an easyp of
any length
}
\description{
Similar to the single version (easyp2dim) but deals with 'meas'
having more than one component. When there are several component, it
is checked that every one is of dimension one: then the dimension is
the number of components.
}
\usage{
easyps2dim(meas, nom)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 easyps2dim(c('{{A[c]}}', '{{A[a]}}*B'), rbsb.nom3);
}
\arguments{
\item{meas}{ character of the complete variable name}
\item{nom}{ referent /nom/.}
}
\value{
dimension in terms of number of variables
}
\keyword{utilities}
