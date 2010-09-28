\name{pause}
\alias{pause}
\title{
pauses the program until an answer is given
}
\description{
This function issues a pause with a message allowing to stop the
process or to continue it.
}
\usage{
pause(what="", where=NULL)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 pause("Time for lunch!");
}
\arguments{
\item{what}{ Message commenting the pause}
\item{where}{ To indicate where the pause was made. When NULL a
shortened message is issued.}
}
\value{
nothing but the actions indicated in the description field are
performed
}
\keyword{IO}
