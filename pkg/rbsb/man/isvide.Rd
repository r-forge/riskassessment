\name{isvide}
\alias{isvide}
\title{
(ba) to avoid difficulty with is.null
}
\description{
returns TRUE is the structure is empty
}
\usage{
isvide(x)
}
\examples{
 isvide(NULL);
}
\arguments{
\item{x}{object to be scrutinazed}
}
\details{
extension of \code{isempty}.
}
\value{
TRUE when the object is considered as empty FALSE if not
}
\keyword{utilities}
