\name{check8alk}
\alias{check8alk}
\title{
checks an /alk/
}
\description{
(ba) This function checks /alk/ more deeply than \code{valid8alk}
which is also called.
}
\usage{
check8alk(object)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 check8alk(rebastaba.alk1);
}
\arguments{
\item{object}{The alk object to be validated.}
}
\details{
After numerous attempts all the checks made in that function where
removed from valid8alk because it seemed difficult to comply the
requirements of a standard package by R checking. As far as possible,
only complementary tests to \code{valid8alk} are performed.\cr The
main idea is that non completed /alk/ must avoid the redundancy while
completed /alk/ must have all slot filled in the standard way.
}
\value{
TRUE when the object seems acceptable else a character describing the
error(s) but when something badly strong is discovered a fatal error
is issued.
}
\keyword{classes}
