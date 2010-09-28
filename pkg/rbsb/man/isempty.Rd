\name{isempty}
\alias{isempty}
\title{
to avoid difficulty with is.null
}
\description{
returns TRUE is the structure is empty
}
\usage{
isempty(x)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 isempty(numeric(0));
 isempty(NULL);
 isempty(rbsb.fau0);
}
\arguments{
\item{x}{object to be scrutinazed}
}
\value{
TRUE when the object is considered as empty FALSE if not
}
\keyword{programming}
