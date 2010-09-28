\name{bn2bugs}
\alias{bn2bugs}
\title{
translates a bn into a bugs model
}
\description{
(bn) /not available for the moment/ exploring all nodes of a bn,
produces a set of lines of comments and commands corresponding to the
definition of the model block into bugs language.
}
\usage{
bn2bugs(bn)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 cat("To be added\n");
}
\arguments{
\item{bn}{bn to translate}
}
\details{
For the moment, only jags dialect is implemented
}
\value{
A character vector, a component a line.
}
