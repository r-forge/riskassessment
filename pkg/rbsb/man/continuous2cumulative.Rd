\name{continuous2cumulative}
\alias{continuous2cumulative}
\title{
cumulative of the continuous part
}
\description{
This function returns the values of the cumulative of a continous
part of an /empirical/ for the values of \code{x}.
}
\usage{
continuous2cumulative(xyco, x, norma=FALSE)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 xx <- seq(0, 6, 0.3);
 xy <- matrix(c(1:5, 0, 1, 3, 3, 0), 5);
 cbind(xx, continuous2cumulative(xy, xx));
}
\arguments{
\item{xyco}{The \code{empirical@xy} to consider.}
\item{x}{ Values for which the values must be computed.}
\item{norma}{Indicates if a normalization to one must be done.}
}
\details{
Values are not normalized, to obtain them just use
\code{normalize8empirical} before extracting the continuous part.
}
\value{
The resulting values.
}
