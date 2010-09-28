\name{des2list}
\alias{des2list}
\title{
transforms a /des/ object into a list.
}
\description{
More or less the reverse of \code{list2des}. When \code{simple} the
shortened way is used.
}
\usage{
des2list(des, simple=FALSE)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 des2list(rbsb.des0);
 des2list(rbsb.des0, FALSE);
}
\arguments{
\item{des}{The /des/ to be transformed into a list.}
\item{simple}{indicates the way the list must be created.}
}
\value{
The generated list
}
\keyword{classes}
