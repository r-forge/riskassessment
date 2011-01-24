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
pause(what="", message=NULL)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 pause("Time for lunch!");
}
\arguments{
\item{what}{ Short message commenting the pause}
\item{message}{ A possible longer message, generally to give details
about from where the pause is issued. When NULL a shortened message
is issued.}
}
\value{
Message(s) are displayed on the screen awaiting for an answer to stop
or continue.
}
\keyword{IO}
