\name{list2win}
\alias{list2win}
\title{
transforms a convenient list into a /win/
}
\description{
(ba) transforms a convenient list into a \code{win} object
}
\usage{
list2win(li)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 list2win(list(nat=c("a", "conti"), swg=c(letters[1:7], 1:7), 
 skk="0", sdi=as.character(0:1), 
 snb=as.character(3:4), rwg="1", rkk="1", 
 rty=c("*", "median")));
 list2win(NULL);
}
\arguments{
\item{li}{the list to be transformed}
}
\value{
the resulting win
}
\keyword{misc}
