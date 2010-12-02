\name{ubeta}
\alias{ubeta}
\title{
adapted draws of a Beta distribution
}
\description{
This function slightly generalizes the basic \code{rbeta} function,
allowing translation and dilatation with \code{support} and
truncation with \code{trunc}. Also the retained parameterization with
expectation \code{mu} and coefficient of variation \code{coefvar}
encompasses some degenerate cases.\cr When the intersection of
\code{support} and \code{trunc} is empty, an error is issued.\cr The
number of returned draws is \code{max(length(mu),length(coefvar))}.
When both lengths are not equal, the smaller one must be equal to
one, if not an error is issued.\cr NA value are possible in \code{mu}
and \code{coefvar}, then missing values are returned for the
considered draws.\cr Be aware that giving some of the \code{mu}
either equal to \code{support[1]} or equal to \code{support[2]} will
imply draws fixed to this value if it belongs to the truncation
interval (\code{NA} otherwise). So an extreme value for \code{mu}
means that it is sure, not compensated by \code{coefvar}. We advise
not to introduce such degenerate cases.
}
\usage{
ubeta(mu, coefvar, support=c(0, 1), trunc=support)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 ubeta(rep(0.8, 20), 2);
 ubeta(c(rep(0.8, 19), NA), 2, support=c(-10, 20));
 ubeta(c(rep(0.8, 19), NA), 100*runif(20), support=c(-10, 10), trunc=c(0.5, 1.5));
}
\arguments{
\item{mu}{numeric vector of the expectation of the beta, must be in
the interval defined by \code{support}.}
\item{coefvar}{numeric vector of the coefficient of variation. Must
be comprised between 0 and 100.}
\item{support}{ The two limits of the interval for which the beta is
defined (equivalent to a translation and dilatation). The support
interval is common for all draws. }
\item{trunc}{The two limits to which distribution is truncated. The
truncation interval is common for all draws. In fact, it is computed
as the intersection with \code{support} which cannot be empty.}
}
\details{
The proposed parameterization is given by the following relationships
with the standard \code{a} and \code{b} parameters when
\code{support=c(0,1)}.\cr \code{mu = a / (a+b)} and \code{coefvar =
200 / (a+b)},\cr \code{a = 200 * (mu / coefvar)} and \code{b = 200 *
((1-mu)/coefvar)}.\cr When \code{mu} is equal to \code{support[1]} or
\code{support[2]}, this is a degenerate case considered as fixed with
\code{coefvar} is null whatever is the provided value.
}
\value{
A vector of the drawn values, possibly containing \code{NA}.
}
