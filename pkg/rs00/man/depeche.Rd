\name{depeche}
\alias{depeche}
\title{
(f0) issues a message on the screen [and returns the answer]
}
\description{
This function issues a message and pause if an answer is required.
}
\usage{
depeche(what="???", answer=TRUE)
}
\examples{
 uu <- depeche("What time is it?");
 print(uu);
}
\arguments{
\item{what}{ Message to be issued}
\item{answer}{ When an answer is awaited.}
}
\value{
the answer as character if answer is TRUE (but no answer is
transformed into "") if not ""
}
\keyword{IO}
