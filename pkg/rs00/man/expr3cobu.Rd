\name{expr3cobu}
\alias{expr3cobu}
\title{
(00) provides a suitable Bugs transcription for translation into Bugs
language
}
\description{
In fact, just removing the quotation for node names
}
\usage{
expr3cobu(x)
}
\arguments{
\item{x}{either a numeric or a rebastaba expression (character)}
}
\details{
The difference with expr3coge is that the expression is adapted for
the Jags coding model without care about the columns of X matrix.
}
\value{
An interpretable character string to be included instead of parameter
for Jags coding
}
\keyword{utilities}
