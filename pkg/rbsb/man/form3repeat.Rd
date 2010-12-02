\name{form3repeat}
\alias{form3repeat}
\title{
prints a repeated given string
}
\description{
Without adding breaking line characters, prints \code{nb} times a
given string of characters
}
\usage{
form3repeat(cha="-", nb=10, imp=FALSE, cr=imp)
}
\examples{
 form3repeat('-+', 20, TRUE)
}
\arguments{
\item{cha}{ The string to repeat}
\item{nb}{ Number of repetitions}
\item{imp}{ Printing when TRUE or returning (default)}
\item{cr}{ Must a line feed be added?}
}
\value{
character string or printing according to \code{imp}
}
\keyword{print}
