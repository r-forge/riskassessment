\name{ulnormal}
\alias{ulnormal}
\title{
adapted draws of a log-normal distribution
}
\description{
This function slightly generalizes the basic \code{rlnorm} function,
allowing a different support that \code{[0,+Inf[} with \code{support}
and truncation with \code{trunc}.\cr The number of returned draws is
\code{max(length(mu),length(coefvar))}. When both lengths are not
equal, the smaller one must be equal to one, if not an error is
issued.\cr NA value are possible in \code{mu} and \code{coefvar},
then missing values are returned for the considered draws.\cr Notice
that \code{support[2]} is not a bound, only its sign is used to know
if the support is \code{[support[1],+Inf[} when plus or
\code{]-Inf,support[1]} when minus.
}
\usage{
ulnormal(mu, coefvar, support=c(0, +1), trunc=c(0, Inf))
}
\examples{
 prr3k("RESET"); # For R checking compliance
 ulnormal(rep(10, 20), 2);
 ulnormal(c(rep(10, 19), NA), 2);
 ulnormal(c(rep(10, 19), NA), 2, trunc=c(8, 12));
}
\arguments{
\item{mu}{numeric vector of the expectation of the normal (i.e. in
the log scale).}
\item{coefvar}{numeric vector of the coefficient of variation (also
in the log scale). Cannot be negative.}
\item{support}{Defines the support of the log-normal distribution.
This is either \code{[support[1],+Inf[} when \code{support[2] >= 0}
or \code{]-Inf,support[1]} when \code{support[2] < 0}. Of course,
this is not in the log scale. The support semi-interval is common for
all draws. }
\item{trunc}{The two limits (not in the log scale) to which the
distribution is truncated. It will be restricted to its intersection
with the support. The truncation interval is common for all draws. }
}
\details{
To comply with /prr/ point of view, the second parameter of the
log-normal distribution is given through the variation
coefficient.\cr In fact, after making the appropriate transformation,
this function calls the \code{unormal} function. To make the code
safer, also the check of the consistency of the parameters is made by
\code{unormal} not by \code{ulnormal}.\cr The remark made for
\code{unormal} applies to this function too (since it is called).
This can generate missing values (\code{NA}) for too small \code{mu}
components.
}
\value{
A vector of the drawn values, possibly containing \code{NA}.
}
