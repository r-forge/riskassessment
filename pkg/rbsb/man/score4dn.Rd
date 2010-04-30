\name{score4dn}
\alias{score4dn}
\title{
(dn) Completes a dn with its score
}
\description{
Completes a dn with its score, that is a last variable is added
domains are computed according to the normal distribution
}
\usage{
score4dn(dn)
}
\examples{
 rsdn3k("RESET"); ## for R checking convenience
 score4dn(rbsb.dn4);
}
\arguments{
\item{dn}{The /dn/ object to be completed.}
}
\value{
the completed /dn/.
}
