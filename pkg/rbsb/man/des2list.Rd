\name{des2list}
\alias{des2list}
\title{
(00) transforms a /des/ object into a list.
}
\description{
More or less the reverse of \code{list2des}.
}
\usage{
des2list(des)
}
\examples{
 rs003k("RESET"); # needed only for R checking, to be forgotten
 des2list(rbsb.des0);
}
\arguments{
\item{des}{The /des/ to be transformed into a list.}
}
\details{
The main use of this function is to tackle alk read from text files
with the function \code{file2list} avoiding the NULL values non
acceptable for slots
}
\value{
The generated list
}
\keyword{classes}
