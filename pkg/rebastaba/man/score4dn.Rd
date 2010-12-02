\name{score4dn}
\alias{score4dn}
\title{
Completes a dn with its score
}
\description{
(dn) Completes a dn with its score, that is a last variable is added
domains are computed according to the normal distribution
}
\usage{
score4dn(dn)
}
\examples{
 rebastaba3k("RESET"); ## for R checking convenience
 score4dn(rebastaba.dn4);
}
\arguments{
\item{dn}{The /dn/ object to be completed.}
}
\value{
the completed /dn/.
}
