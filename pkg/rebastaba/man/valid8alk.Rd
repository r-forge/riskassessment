\name{valid8alk}
\alias{valid8alk}
\title{
checks an /alk/
}
\description{
(ba) This function checks /alk/ objects in a light way. For a more
complete checking one must use \code{check8alk}
}
\usage{
valid8alk(object)
}
\arguments{
\item{object}{The alk object to be validated.}
}
\details{
It is the validity method for /alk/ objects.\cr The main idea is that
non completed /alk/ must avoid the redundancy while completed /alk/
must have all slot filled in the standard way.
}
\value{
TRUE when the object seems acceptable else a character describing the
error(s). Sometimes, when the inconsistency seems too severe, a fatal
error can be issued.
}
\keyword{classes}
