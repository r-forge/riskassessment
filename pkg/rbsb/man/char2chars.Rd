\name{char2chars}
\alias{char2chars}
\title{
transforms a single character into a character vector
}
\description{
Just doing a character vector from a single character using
\code{sep} to separate the different components.
}
\usage{
char2chars(cara, sep=rbsb.sep0, rsep="no_action")
}
\examples{
 rbsb3k("reset"); # for R checking convenience
 char2chars("A B C");
 char2chars("A B C", " ");
 char2chars("A B C ");
 char2chars("A B C ", rsep="");
 char2chars("0.1 0.9 // 0.5 0.5 // 0.9 0.1", "//");
}
\arguments{
\item{cara}{ The single character to transform.}
\item{sep}{ Separator for the splitting when '', no splitting is
done.}
\item{rsep}{ Indicates if repetitions of \code{sep0} must be
considered as signicant or not. If \code{no_action} then their
repetitions will be ignored if not \code{character(0)} components
will be introduced.}
}
\details{
Argument \code{sep} is a possible separator. If empty no splitting is
performed.
}
\value{
a character vector if everything was good. a 'faux' object otherwise.
}
\keyword{utilities}
