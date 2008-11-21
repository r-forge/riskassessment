\name{mcapply}
\alias{mcapply}
\title{
Apply Functions Over mc or mcnode Objects
}
\description{
Apply a function on all values or over a given dimension of an
\code{mcnode} object. May be used for all \code{mcnode} of an
\code{mc} object.
}
\usage{
mcapply(x, margin=c("all", "var", "unc", "variates"), fun, ...)
}
\examples{
data(total)
xVUM
mcapply(xVUM, "unc", sum)
mcapply(xVUM, "var", sum)
mcapply(xVUM, "all", sum)
mcapply(xVUM, "variates", sum)
mcapply(total, "all", exp)
}
\arguments{
\item{x}{A \code{mc} or a \code{mcnode} object.}
\item{margin}{The dimension on which applying the function. Maybe
\code{"all"} (default) to apply the function on all values,
\code{"var"} to apply the function on the variability dimension,
\code{"unc"} to apply the function on the uncertainty dimension, or
\code{"variates"} to apply the function on the variates.}
\item{fun}{The function to be applied. When applied to a vector of
length \code{n}, \code{fun} should return a vector of length \code{n}
or \code{1}.}
\item{\dots}{Optionnal arguments to \code{fun}.}
}
\value{
If \code{fun} returns a function of length \code{n} or if
\code{margin="all"}, the returned \code{mcnode}s are of type and
dimension of \code{x}. In other cases, the type of \code{mcnode} is
changed.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{apply}}, \code{\link{mc}}, \code{\link{mcnode}}.
}
\keyword{misc}
