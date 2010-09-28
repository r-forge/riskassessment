\name{daf2list}
\alias{daf2list}
\title{
transforms a /daf/ into a list
}
\description{
This function transforms a /daf/ object into a standard list. The
shortened way is used when \code{simple}.
}
\usage{
daf2list(daf, simple=TRUE)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 daf2list(rbsb.daf1);
 daf2list(NULL);
 daf2list(rbsb.daf1, FALSE);
}
\arguments{
\item{daf}{The daf object to be transformed.}
\item{simple}{indicates the way the list must be created.}
}
\value{
returns the resulting list
}
\keyword{misc}
