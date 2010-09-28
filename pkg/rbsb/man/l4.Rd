\name{l4}
\alias{l4}
\title{
trick for loops
}
\description{
returns 1:length(x) when length(x) \eqn{>}0 and numeric(0) otherwise.
Quite useful to prevent starting a loop of length nought
}
\usage{
l4(x)
}
\examples{
 l4(0);
 l4(5);
 l4(character(0));
 l4(letters);
}
\arguments{
\item{x}{vector}
}
\value{
1:length(x) if length(x) \eqn{>} 0 else numeric(0)
}
\keyword{iteration}
