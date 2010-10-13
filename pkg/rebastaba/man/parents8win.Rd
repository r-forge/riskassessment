\name{parents8win}
\alias{parents8win}
\title{
finds the parents at the variable level of a /win/
}
\description{
(ba) finds the variable parents indicated by a \code{win}. Notice
that, contrary to a previous version, the possible monitoring
variable is not considered as a parent; it must be comprised in the
empidata.
}
\usage{
parents8win(win)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 parents8win(rebastaba.win1);
}
\arguments{
\item{win}{win object}
}
\details{
The finding of the parents is done by analyzing the different fields
of the /win/. This is a basic function, no check is performed.
}
\value{
the names of the /win/ parents at the variable level.
}
\keyword{misc}
