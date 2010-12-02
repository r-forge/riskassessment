\name{unormal}
\alias{unormal}
\title{
adapted draws of a normal distribution
}
\description{
This function slightly generalizes the basic \code{rnorm} function,
allowing truncation with \code{trunc}.\cr The number of returned
draws is \code{max(length(mu),length(coefvar))}. When both lengths
are not equal, the smaller one must be equal to one, if not an error
is issued.\cr \code{NA} value are possible in \code{mu} and
\code{coefvar}, then missing values are returned for the considered
draws.
}
\usage{
unormal(mu, coefvar, trunc=c(-Inf, Inf))
}
\examples{
 prr3k("RESET"); # For R checking compliance
 unormal(rep(10, 20), 2);
 unormal(c(rep(10, 19), NA), 2);
 unormal(c(rep(10, 19), NA), 2, trunc=c(8, 12));
}
\arguments{
\item{mu}{numeric vector of the expectation of the normal.}
\item{coefvar}{numeric vector of the coefficient of variation. Cannot
be negative.}
\item{trunc}{The two limits to which distribution is truncated. The
truncation interval is common for all draws. }
}
\details{
To comply with /prr/ point of view, the second parameter of the
normal distribution is given through the variation coefficient. The
relation with more standard sigma is given by\cr sigma =
(coefvar/100)*abs(mu) and\cr coefvar = (100*sigma) / abs(mu). Notice
that as the standard deviation is given through the variation
coefficient, expectation values too close to zero can be problematic.
This is why, such values are discarded and replaced by \code{NA}. A
local constant is used for the test.
}
\value{
A vector of the drawn values, possibly containing \code{NA}.
}
