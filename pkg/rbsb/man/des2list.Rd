\name{des2list}
\alias{des2list}
\title{
transforms a /des/ object into a list.
}
\description{
Transforms a /des/ object into a list. More or less the reverse of
\code{list2des}. When \code{simple} the shortened way is used.
}
\usage{
des2list(des, simple=FALSE)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 des2list(rbsb.des1);
 des2list(rbsb.des1, TRUE);
}
\arguments{
\item{des}{The /des/ to be transformed into a list.}
\item{simple}{indicates the way the list must be created.}
}
\details{
Simple way means that the description is given as a character vector,
not as a list. See the proposed examples.
}
\value{
The generated list
}
\keyword{classes}
