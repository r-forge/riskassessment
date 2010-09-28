\name{easyp2dim}
\alias{easyp2dim}
\title{
returns the dimension of the variable defined in an easyp
character(1)
}
\description{
(ba) Decoding the node/variables of ONE easyp, returns the dimension
of the associated variable according to the associated /nom/.
}
\usage{
easyp2dim(eas, nom)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 easyp2dim('{{A}}', rbsb.nom2);
 easyp2dim('{{A[c]}}', rbsb.nom3);
 easyp2dim('{{A[a]}}*{{C}}', rbsb.nom3);
}
\arguments{
\item{eas}{ character of the complete variable name}
\item{nom}{ referent /nom/.}
}
\value{
dimension in terms of number of variables
}
\keyword{utilities}
