\name{trapezoid2empirical}
\alias{trapezoid2empirical}
\title{
returns the /empirical/ associated to a trapezoid distribution
}
\description{
This function returns the /empirical/ object corresponding to the
trapezoid distribution where \code{a} is the minimum value, \code{b}
and \code{c} are the mode values and \code{d} is the maximum value.
}
\usage{
trapezoid2empirical(a=-1, b=-0.5, c=0.5, d=1, des=NULL)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 plot(trapezoid2empirical(1, 5, 6, 6), what="d");
}
\arguments{
\item{a}{Minimal value}
\item{b}{Beginning of the mode interval}
\item{c}{Ending of the mode interval}
\item{d}{Maximal value}
\item{des}{ /des/ to give to the object, when \code{NULL} a standard
description will be provided.}
}
\details{
The resulting object is normalized.
}
\value{
An /empirical/ object.
}
