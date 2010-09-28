\name{lp}
\alias{lp}
\title{
trick for loops
}
\description{
returns 1:nb when nb \eqn{>}0 and numeric(0) otherwise. Quite useful
to prevent starting a loop of length nought
}
\usage{
lp(nb)
}
\examples{
 lp(0);
 lp(5);
}
\arguments{
\item{nb}{integer}
}
\value{
1:nb if nb \eqn{>} 0 else numeric(0)
}
\keyword{iteration}
