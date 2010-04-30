\name{check8alk}
\alias{check8alk}
\title{
(ba) checks an /alk/
}
\description{
This function checks /alk/ objects much more deeply than does
valid8alk
}
\usage{
check8alk(object)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 check8alk(rbsb.alk1);
}
\arguments{
\item{object}{The alk object to be validated.}
}
\details{
After numerous attempts all the checks made in that function where
removed from valid8alk because it seemed difficult to comply the
requirements of a standard package by R checking.
}
\value{
TRUE when the object seems acceptable else a character describing the
error(s)
}
\keyword{classes}
