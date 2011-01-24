\name{form3nume}
\alias{form3nume}
\title{
provides a systematic numbering of items
}
\description{
Provides systematic names for a series of items according their
number giving them an identical number of character.
}
\usage{
form3nume(nbn, type="n")
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 form3names(27);
 form3names(100, "a");
}
\arguments{
\item{nbn}{Number of new item names to generate}
\item{type}{ Defines the type of numbering to use. There are three
possibilities: \code{A} for uppercased letters, \code{a} for
lowercased letters and \code{n} for arabic numbers.}
}
\details{
(see the code)
}
\value{
vector with \code{nbn} different strings associated to new names
}
\keyword{utilities}
