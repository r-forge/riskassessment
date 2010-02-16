\name{form3ind}
\alias{form3ind}
\title{
(00) provides indentation of different levels
}
\description{
The level of indentation is given by \code{niv*rbsb.min} times the
character ele. When cr, a line feed is made at first.
}
\usage{
form3ind(niv=1, cr=TRUE, ele="")
}
\examples{
 rs003k("RESET"); # needed only for R checking, to be forgotten
 cat(form3ind(2), "Bien `a vous", "\n");
}
\arguments{
\item{niv}{ Level of indentation.}
\item{cr}{ Must a line feed provided first?}
\item{ele}{ String to use.}
}
\value{
a scalar string
}
\keyword{print}
