\name{bf}
\alias{bf}
\title{
trick for loops
}
\description{
returns 1:length(x) when length(x) \eqn{>}0 and numeric(0) otherwise.
Quite useful to prevent starting a loop of length nought
}
\usage{
bf(x)
}
\examples{
 bf(0);
 bf(5);
 bf(character(0));
 bf(letters);
}
\arguments{
\item{x}{vector}
}
\value{
1:length(x) if length(x) \eqn{>} 0 else numeric(0)
}
\keyword{iteration}
