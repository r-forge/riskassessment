\name{form3affiche}
\alias{form3affiche}
\title{
displays with its name any object
}
\description{
displays any object after giving the name of the variable containing
it. A pause can be introduced to give the opportunity to scrutinize
the result.
}
\usage{
form3affiche(x, pau=FALSE, cat=FALSE, ...)
}
\examples{
 uu <- "azerty";
 form3affiche(uu);
}
\arguments{
\item{x}{The object to print.}
\item{pau}{ Must a pause be performed after the display?}
\item{cat}{ Must the printing be done with 'cat' instead of print?}
\item{\dots}{possible arguments for the print function.}
}
\value{
a print (or cat) is done and \code{x} is returned
}
\keyword{print}
