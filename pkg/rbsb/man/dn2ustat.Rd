\name{dn2ustat}
\alias{dn2ustat}
\title{
(dn) Computes the univariate statistics for the variables of a /dn/
}
\description{
Computes the univariate statistics for the variables of a /dn/
storing the result in a list of names: the components of
\code{rbsb.sna}.
}
\usage{
dn2ustat(dn, qui=rbsb.num0)
}
\examples{
 rsdn3k("RESET"); ## for R checking convenience
 dn2ustat(rbsb.dn2);
 dn2ustat(rbsb.dn4);
}
\arguments{
\item{dn}{The /dn/ object.}
\item{qui}{(\code{numeric} or \code{character}) indicates the
variable on which it must be computed. default, all the variates.}
}
\value{
A list for each type of variable natures, each component providing
standard statistics.
}
