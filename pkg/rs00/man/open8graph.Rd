\name{open8graph}
\alias{open8graph}
\title{
(f0) opens the graph device for rebastaba
}
\description{
According to the global constant rbsb3k("mfi") a graphical device is
open or not. Must be called before plotting something ones want to
keep under rbsb3k("mgr") type. This global constant can take only
value in c("pdf","png").\cr
}
\usage{
open8graph(prefix=NULL, ...)
}
\examples{
 rbsb3k('*', 'std');
 rbsb3k('ffg', 'print');
 open8graph();
 close8graph();
 rbsb3k('ffg', 'print');
}
\arguments{
\item{prefix}{ When NULL the \code{1+rbsb3k("ffg")} numeric is taken
to give the number of the file with three digits. If not, it is a
character giving the complete prefix to use before the suffix.}
\item{\dots}{ argument(s) to be transmitted to the openning device of
graphics. Quite useful for specific character and picture sizes, or
to get more than one graph into the file.}
}
\details{
The file opened for storing the graph is named with three components
separated with dots: rbsb3k("fpx"), prefix and rbsb3k("mgr").
rbsb3k("fpx") is a prefix to be modified by the user as convenient.
rbsb3k("ffg") is the number of the current figure incremented by this
function. rbsb3k("mgr") is the suffix associated to the type of graph
(either 'pdf' or 'png').
}
\value{
Nothing but when rbsb3k("mfi") is \code{TRUE} the and
\code{is.null(prefix)}, the graphical device is open and the global
constant rbsb3k("ffg") is increased *before* with one.
}
\keyword{IO}
