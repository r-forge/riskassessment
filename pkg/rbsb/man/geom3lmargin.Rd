\name{geom3lmargin}
\alias{geom3lmargin}
\title{
adds some padding to a range
}
\description{
Used for the preparation of graphic outputs: the range of \code{x} is
extended with a proportion \code{ma} at both ends when
\code{diff(range(x))} is not nought, else of \code{pro*x[1]}.
}
\usage{
geom3lmargin(x, ma=0.15, pro=0.10)
}
\examples{
 geom3lmargin(1:5);
 geom3lmargin(1:5, 0.1);
}
\arguments{
\item{x}{vector of values to consider}
\item{ma}{proportion margin to add from the range of value.}
\item{pro}{proportion margin to add from the range of values when the
range is of lenght zero.}
}
\value{
A vector of two numerical giving the extended range
}
\keyword{utilities}
