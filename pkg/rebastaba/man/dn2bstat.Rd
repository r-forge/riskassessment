\name{dn2bstat}
\alias{dn2bstat}
\title{
Computes the bivariate statistics for the variables of a /dn/
}
\description{
(dn) Computes the bivariate statistics for the variables of a /dn/
The result is given by a pseudo-correlation matrix.
}
\usage{
dn2bstat(dn, qui=rbsb.num0)
}
\examples{
 rebastaba3k("RESET"); ## for R checking convenience
 rebastaba.mnb <- 8;
 dn2bstat(rebastaba.dn2);
 dn2bstat(rebastaba.dn4);
}
\arguments{
\item{dn}{The /dn/ object.}
\item{qui}{(\code{numeric} or \code{character}) indicates the
variable on which it must be computed. default, all the variates.}
}
\details{
See \code{df2bstat} for the details.
}
\value{
A symmetric matrix of 'correlations'.
}
