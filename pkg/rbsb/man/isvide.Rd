\name{isvide}
\alias{isvide}
\title{
to avoid difficulty with is.null
}
\description{
(ba) returns TRUE is the structure is null, empty or vide. For this
last case, it is directed to S4 objects with boolean slot @vide.
}
\usage{
isvide(x)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 isvide(NULL);
 isvide(rbsb.fau1);
 isvide(rbsb.fau0);
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
