\name{normalize8empirical}
\alias{normalize8empirical}
\title{
normalizes an /empirical/
}
\description{
This function normalizes an /empirical/ object, that is returns the
object with weight transformed into probabilities (taking care of the
two parts)
}
\usage{
normalize8empirical(empi)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 print(normalize8empirical(new("empirical")));
 e1 <- new("empirical", des=char2des("e1"), sup=c(0, 10), repa=c(10, 0), xw=matrix(c(1, 2, 1, 2, 1, 1:5), ncol=2));
 print(e1);
 print(normalize8empirical(e1));
 e2 <- new("empirical", des=char2des("e2"), sup=c(2.5, 3.5), repa=c(0, 5), xy=matrix(c(1:5, 0, 1, 1, 1, 0), ncol=2));
 print(e2);
 print(normalize8empirical(e2));
}
\arguments{
\item{empi}{The \code{empirical} object to normalize.}
}
\details{
Also a sorting of the discrete values is performed and multiple
identical values are merged. Zero values are eliminated as much as
possible. Also are values outside the proposed support.
}
\value{
the transformed /empirical/
}
\keyword{classes}
