\name{open8graph}
\alias{open8graph}
\title{
(00) opens the graph device for rebastaba
}
\description{
According to the global constant rbsb.mfi a graphical device is open
or not. Must be called before plotting something ones want to keep
under rbsb.mgr type. This global constant can take only value in
c("pdf","png").\cr
}
\usage{
open8graph(prefix=NULL, ...)
}
\examples{
 rs003k("RESET"); # needed only for R checking, to be forgotten
 print(rbsb.ffg);
 open8graph();
 close8graph();
 print(rbsb.ffg);
}
\arguments{
\item{prefix}{ When NULL the \code{1+rbsb.ffg} numeric is taken to
give the number of the file with three digits. If not, it is a
character giving the complete prefix to use before the suffix.}
\item{\dots}{ argument(s) to be transmitted to the openning device of
graphics. Quite useful for specific character and picture sizes, or
to get more than one graph into the file.}
}
\details{
The file opened for storing the graph is named with three components
separated with dots: rbsb.fpx, prefix and rbsb.mgr. rbsb.fpx is a
prefix to be modified by the user as convenient. rbsb.ffg is the
number of the current figure incremented by this function. rbsb.mgr
is the suffix associated to the type of graph (either 'pdf' or
'png').
}
\value{
Nothing but when rbsb.mfi is \code{TRUE} the and
\code{is.null(prefix)}, the graphical device is open and the global
constant "rbsb.ffg" is increased *before* with one.
}
\keyword{IO}
