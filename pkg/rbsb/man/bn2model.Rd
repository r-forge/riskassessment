\name{bn2model}
\alias{bn2model}
\title{
(bn) expresses the model formulae
}
\description{
just returning a character string with the model
}
\usage{
bn2model(bn, comment=1)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 bn2model(rbsb.bn2);
 bn2model(rbsb.bn1);
}
\arguments{
\item{bn}{The rbsb object}
\item{comment}{(=1) 0: iin, 1:nom (for the identification of the
variables)}
}
\details{
separators can be easily changed if necessary since they are coded as
constants
}
\value{
character string
}
