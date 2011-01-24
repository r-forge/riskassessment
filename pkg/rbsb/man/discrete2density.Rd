\name{discrete2density}
\alias{discrete2density}
\title{
density of the discrete part
}
\description{
This function returns the density of a discrete part of an
/empirical/ for the values of \code{x}.
}
\usage{
discrete2density(xwdi, x)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 xx <- c(4, 6, 3);
 xw <- matrix(c(1:3, 3:4, 0, 1, 3, 3, 0), 5);
 cbind(xx, ww=discrete2density(xw, xx));
}
\arguments{
\item{xwdi}{The \code{empirical@xw} to consider.}
\item{x}{ Values for which the values must be computed.}
}
\details{
Values are not normalized, to obtain them just use
\code{normalize8empirical} before extracting the continuous part.
}
\value{
The resulting values. If the \code{xwdi} was not normalized, possible
multiple defined weight are cumulated.
}
