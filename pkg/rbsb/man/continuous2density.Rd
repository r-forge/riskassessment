\name{continuous2density}
\alias{continuous2density}
\title{
density of the continuous part
}
\description{
This function returns the density of a continous part of an
/empirical/ for the values of \code{x}.
}
\usage{
continuous2density(xyco, x, dupli="m")
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 xx <- seq(0, 6, 0.3);
 xy <- matrix(c(1:5, 0, 1, 3, 3, 0), 5);
 cbind(xx, continuous2density(xy, xx));
}
\arguments{
\item{xyco}{The \code{empirical@xy} to consider.}
\item{x}{ Values for which the values must be computed.}
\item{dupli}{ When a \code{x} value is exactly in a vertical part of
the density. Several choices are possible monitored by \code{dupli}.
If "d", the point will be duplicated, "1", the first value will be
chosen, "2", the second value will be chosen, "m", the mean value
will be chosen.}
}
\details{
Values are not normalized, to obtain them just use
\code{normalize8empirical} before extracting the continuous part.
}
\value{
The resulting values. When \code{dupli=="d"}, they are not returned
as a vector but as a matrix the two columns of which are the possibly
duplicated \code{x} and the resulting values.
}
