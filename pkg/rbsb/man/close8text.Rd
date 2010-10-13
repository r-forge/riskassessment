\name{close8text}
\alias{close8text}
\title{
pauses (and more) the program until an answer is given
}
\description{
Closing the output file \code{rbsb.fou} according to \code{rbsb.mfi}.
A pause allowing to stop the process is issued if \code{rbsb.mba} is
false.
}
\usage{
close8text(message=rbsb.cha0)
}
\arguments{
\item{message}{ Message to display on the terminal. When empty, no
message will be displayed.}
}
\value{
nothing but the actions indicated in the description field are
performed
}
\keyword{IO}
