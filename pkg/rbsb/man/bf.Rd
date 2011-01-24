\name{bf}
\alias{bf}
\title{
sequence for looping over an object
}
\description{
This function returns \code{1:length(x)} when \code{length(x) > 0}
and \code{numeric(0)} otherwise. Quite useful to prevent starting a
loop of length nought
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
\code{1:length(x)} if \code{length(x) > 0} else \code{numeric(0)}.
}
\keyword{iteration}
