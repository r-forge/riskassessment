\name{erreur}
\alias{erreur}
\title{
issues an error message and concludes accordingly
}
\description{
when called this function prints x, then displays a message before
stopping the process except if it is a warning or if the general
constant of rebastaba rbsb.mfa is true.
}
\usage{
erreur(x, ..., w=FALSE)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 erreur(matrix(1:4, 2), "This matrix is not symmetric", w=TRUE)
 erreur("Are you sure of it?", w=TRUE);

}
\arguments{
\item{x}{object to be printed before the message. When NULL or "",
nothing is printed. When it is a list, all components of the list are
successively printed.}
\item{\dots}{pieces of message to display after pasting}
\item{w}{ Indicates if it is a simple warning}
}
\value{
nothing
}
\keyword{error}
