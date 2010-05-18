\name{sjl}
\alias{sjl}
\title{
(00) trick for loops
}
\description{
returns 1:length(x) when length(x) \eqn{>}0 and numeric(0) otherwise.
Quite useful to prevent starting a loop of length nought
}
\usage{
sjl(x)
}
\examples{
 sjl(0);
 sjl(5);
 sjl(character(0));
 sjl(letters);
}
\arguments{
\item{x}{vector}
}
\value{
1:length(x) if length(x) \eqn{>} 0 else numeric(0)
}
\keyword{iteration}
