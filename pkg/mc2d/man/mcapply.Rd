\name{mcapply}
\alias{mcapply}
\title{
Apply Functions Over mc or mcnode Objects
}
\description{
Apply a function on all values or over a given dimension of an mcnode
object. May be used for all mcnode of an mc object.
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
\item{x}{A mc or a mcnode object.}
\item{margin}{The dimension on which applying the function. Maybe
"all" (default) to apply the function on all values, "var" to apply
the function on the variability dimension, "unc" to apply the
function on the uncertainty dimension, or "variates" to apply the
function on the variates.}
\item{fun}{The function to be applied. When applied to a vector of
length n, fun should return a vector of length n or 1.}
\item{\dots}{Optionnal arguments to fun.}
}
\value{
If fun returns a function of length n or if margin=="all", the
returned mcnodes are of type and dimension of x. In other cases, the
type of mcnode is changed.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{apply}}, \code{\link{mc}}, \code{\link{mcnode}}.
}
\keyword{misc}
