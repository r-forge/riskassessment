\name{ucateg}
\alias{ucateg}
\title{
draws from a categorical distribution
}
\description{
Following the description given in /prr/ document, returns draws from
a so-called categorical distribution defined by the \code{lim} vector
of limits. The number of limits must be greater or equal to 3. The
returned values are in between the two extreme limits.\cr The number
of returned draws is \code{max(length(mu),length(coefvar))}. When
both lengths are not equal, the smaller one must be equal to one, if
not an error is issued.\cr NA value are possible in \code{mu} and
\code{coefvar}, then missing values are returned for the considered
draws.
}
\usage{
ucateg(mu, coefvar, lim)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 set.seed(1234); 
 ucateg(round(10*runif(25)), 30, 1:10);
}
\arguments{
\item{mu}{numeric vector of the expectations. From it the central
limit is choosen.}
\item{coefvar}{numeric vector of coefficients of variation. Either a
common value for every draws, or a value for each of the draws.
Cannot be negative.}
\item{lim}{vector of the ordered limits; at least three limits. The
limits are common for all draws. }
}
\details{
Extensive use is made of the function \code{ubeta}.
}
\value{
A vector of the drawn values, possibly containing \code{NA}.
}
