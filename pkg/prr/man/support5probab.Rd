\name{support5probab}
\alias{support5probab}
\title{
extracts the parents of a /probab/
}
\description{
returns the support of a given /probab/.
}
\usage{
support5probab(probab)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 support5probab(prr.probab1);
 support5probab(prr.probab2);
 support5probab(prr.probab3);
}
\arguments{
\item{probab}{The /probab/ to be considered.}
}
\details{
\code{@pafixe} is interpreted according to the family
}
\value{
A \code{numeric(2)} defining the support.
}
