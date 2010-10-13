\name{bc}
\alias{bc}
\title{
sequence for a loop
}
\description{
This function returns \code{1:nb} when \code{nb > 0} and
\code{numeric(0)} otherwise.\cr Quite useful to prevent starting a
loop of length nought
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
\code{1:nb} if \code{nb > 0} else \code{numeric(0)}.
}
\keyword{iteration}
