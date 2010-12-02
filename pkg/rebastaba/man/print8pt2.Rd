\name{print8pt2}
\alias{print8pt2}
\title{
prints a pt object of dimension two
}
\description{
(ba) This function prints a two-way probability table as a joint
distribution, a conditional in both sides. This is quite peculiar
function to be generalized later on. BE AWARE that the 'pt' must
comprise the joint distribution.
}
\usage{
print8pt2(pt, comment=0, kkk=-3, tra=FALSE)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 uu <- rebastaba.pta1;
 print8pt2(uu, kkk=3);
}
\arguments{
\item{pt}{The pt object.}
\item{comment}{ Numeric vector defining the way to print. 0: joint
probability, 1: conditional by rows, 2: conditional by columns.}
\item{kkk}{ the coefficient for rounding the probabilities.}
\item{tra}{ Must the table be transposed when printed?}
}
\value{
nothing but a print is performed
}
\keyword{print}
