\name{bc}
\alias{bc}
\title{
trick for loops
}
\description{
returns 1:nb when nb \eqn{>}0 and numeric(0) otherwise. Quite useful
to prevent starting a loop of length nought
}
\usage{
bc(nb)
}
\examples{
 bc(0);
 bc(5);
}
\arguments{
\item{nb}{integer}
}
\value{
1:nb if nb \eqn{>} 0 else numeric(0)
}
\keyword{iteration}
