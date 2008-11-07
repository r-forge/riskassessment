\name{mc.control}
\alias{mc.control}
\alias{ndvar}
\alias{ndunc}
\title{
Sets or Gets the Default Number of Simulations.
}
\description{
Sets or retrieves the default number of simulations.
}
\usage{
ndvar(n)
ndunc(n)
}
\examples{
(oldvar <- ndvar())
(oldunc <- ndunc())
mcstoc(runif, type="VU")
ndvar(12)
ndunc(21)
mcstoc(runif, type="VU")
ndvar(oldvar)
ndunc(oldunc)

}
\arguments{
\item{n}{Number of simulations.}
}
\details{
\code{ndvar()} gets and \code{ndvar(n)} sets the default number of
simulation in the 1D simulations or the number of simulation in the
variability dimension in the 2D simulations.

 \code{ndunc()} gets and \code{ndunc(n)} sets the number of
simulations in the uncertainty dimension in the 2D simulations.

 \code{n} is rounded to its ceiling value.

 The default values when loaded are 1001 for \code{ndvar} and 101 for
\code{ndunc}.
}
\value{
The current value, AFTER modification if \code{n} is present (!=
\code{options}).
}
\author{
Regis Pouillot
}
\keyword{misc}
