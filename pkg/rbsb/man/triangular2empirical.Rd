\name{triangular2empirical}
\alias{triangular2empirical}
\title{
returns the /empirical/ associated to a triangular distribution
}
\description{
This function returns the /empirical/ object corresponding to the
triangular distribution where \code{a} is the minimum value, \code{b}
is the mode value and \code{c} is the maximum value.
}
\usage{
triangular2empirical(a=-1, b=0, c=1, des=NULL)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 plot(triangular2empirical(1, 5, 6), what="d");
}
\arguments{
\item{a}{Minimal value}
\item{b}{Most probable value}
\item{c}{Maximal value}
\item{des}{ /des/ to give to the object, when \code{NULL} a standard
description will be provided.}
}
\details{
The resulting object is normalized.
}
\value{
An /empirical/ object.
}
