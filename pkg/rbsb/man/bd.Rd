\name{bd}
\alias{bd}
\title{
sequence for insertions
}
\description{
This function returns \code{n1:n2} when \code{n1<=n2} and
\code{numeric(0)} otherwise.\cr Quite useful when some insertion must
be done within a sequence
}
\usage{
bd(n1, n2)
}
\examples{
 xx <- 1:5;
 for (ii in 1:6) { print(c(xx[bd(1, ii-1)], 10, xx[bd(ii, 5)]));}
}
\arguments{
\item{n1}{first element}
\item{n2}{second element}
}
\value{
\code{n1:n2} if \code{n1<n2} else \code{numeric(0)}.
}
\keyword{iteration}
