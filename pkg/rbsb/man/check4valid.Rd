\name{check4valid}
\alias{check4valid}
\title{
checks the returned value of valid8 functions
}
\description{
When \code{x} is character, then it is printed as well as
\code{message} and according to \code{fatal} the program can be
stopped.
}
\usage{
check4valid(x, message=NULL, fatal=TRUE)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 check4valid("That's not good enough", fatal=FALSE);
 check4valid(TRUE);
 check4valid("A", "B C and so on.", fatal=FALSE);
}
\arguments{
\item{x}{object to be checked.}
\item{message}{ Some additional message to be issued before
stopping.}
\item{fatal}{ what to do when discovering a character? TRUE: this
function prints the message and stops; FALSE: this function returns
the message as a character.}
}
\value{
When the check is validated returns TRUE. If not, according to
\code{fatal} prints the message and stops or returns the message.
}
\keyword{error}
