\name{list2vect}
\alias{list2vect}
\title{
(f0) transforms a list of scalars into a (named) vector
}
\description{
All components of the list are supposed to be scalars (if not a fatal
error is issued. When numeric type is asked and the conversion is not
possible, NA is returned.
}
\usage{
list2vect(li, wh="C", name.rm=TRUE)
}
\examples{
 list2vect(list(a="AAA", b="BBB"));
}
\arguments{
\item{li}{The list to deal with.}
\item{wh}{ Type of conversion: 'C' means character, 'N' means
numeric.}
\item{name.rm}{ Indicates when it exists the removing of item "name"
(to be consistent with read8list.}
}
\value{
a unique vector concatening all components of the list with the
desired type.
}
\keyword{utilities}
