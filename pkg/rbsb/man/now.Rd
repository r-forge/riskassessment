\name{now}
\alias{now}
\title{
returns a character giving the present moment
}
\description{
Returns a character giving the present moment with different
components
}
\usage{
now(what="dh", format="red")
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 cat("Now is", now(), "\n");
 cat(now("dhms", "verbose"), "\n"); 
}
\arguments{
\item{what}{\code{character(1)} indicating which components to
include among: \cr\code{d} for the day, \cr\code{h} for the hour,
\cr\code{m} for the minute, \cr\code{s} for the second.}
\item{format}{\code{"red"} for reduced if not in a verbose way.}
}
\details{
based on Sys.time R function
}
\value{
a character
}
\keyword{utilities}
