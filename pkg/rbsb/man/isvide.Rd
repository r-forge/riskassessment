\name{isvide}
\alias{isvide}
\title{
to avoid difficulty with is.null
}
\description{
(ba) returns TRUE is the structure is null, empty or vide. For this
last case, their list is given by \code{rbsb.null} which is increased
with new objects by the children packages.\cr Any object being null
for \code{isempty} is for \code{isvide}. \cr Notice that
\code{isvide("")} is FALSE.
}
\usage{
isvide(x)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 isvide(NULL);
 isvide(rbsb.fau1);
 isvide(rbsb.fau0);
 isvide("");
}
\arguments{
\item{x}{object to be scrutinazed}
}
\details{
Have a look to the code itself.
}
\value{
TRUE when the object is considered as not filled, FALSE if not.
}
\keyword{utilities}
