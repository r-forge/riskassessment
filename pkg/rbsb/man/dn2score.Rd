\name{dn2score}
\alias{dn2score}
\title{
(dn) Computes the individual scores for a /dn/
}
\description{
Using the \code{rbsb.scv} limits computes according to the nature of
each variable an 'outlying' score for each row of \code{dn@df}.
}
\usage{
dn2score(dn, qui=rbsb.num0)
}
\examples{
 rsdn3k("RESET"); ## for R checking convenience
 dn2score(rbsb.dn2);
 dn2score(rbsb.dn4, 4);
 dn2score(rbsb.dn4, 5);
 dn2score(rbsb.dn4, 6);
}
\arguments{
\item{dn}{The /dn/ object.}
\item{qui}{(\code{numeric} or \code{character}) indicates the
variable on which it must be computed. default, all the variates.}
}
\value{
A numeric vector containing the scores in the right order
}
