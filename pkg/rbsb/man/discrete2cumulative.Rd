\name{discrete2cumulative}
\alias{discrete2cumulative}
\title{
cumulative of the discrete part
}
\description{
This function returns the values of the cumulative of a discrete part
of an /empirical/ for the values of \code{x}.
}
\usage{
discrete2cumulative(xwdi, x, norma=FALSE)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 xx <- c(4, 6, 3);
 xw <- matrix(c(1:3, 3:4, 0, 1, 3, 3, 0), 5);
 cbind(xx, ww=discrete2cumulative(xw, xx));
}
\arguments{
\item{xwdi}{The \code{empirical@xw} to consider.}
\item{x}{ Values for which the values must be computed.}
\item{norma}{Indicates if a normalization to one must be done.}
}
\details{
Values are not normalized, to obtain them just use
\code{normalize8empirical} before extracting the continuous part.
}
\value{
The resulting values. If the \code{xwdi} was not normalized, possible
multiple defined weight are cumulated.
}
