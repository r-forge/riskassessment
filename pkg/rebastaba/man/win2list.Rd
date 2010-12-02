\name{win2list}
\alias{win2list}
\title{
transforms into a list a /win/
}
\description{
(ba) transforms into a list a \code{win} object
}
\usage{
win2list(win)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 win2list(new("win", nat=structure(rep("categ", 7), .Names=letters[1:7])));
 win2list(NULL);
}
\arguments{
\item{win}{the win object to be transformed}
}
\value{
a list of vectors
}
\keyword{misc}
