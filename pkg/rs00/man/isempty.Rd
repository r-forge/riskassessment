\name{isempty}
\alias{isempty}
\title{
(f0) to avoid difficulty with is.null
}
\description{
returns TRUE is the structure is empty
}
\usage{
isempty(x)
}
\examples{
 isempty(numeric(0));
 isempty(NULL);
 isempty(rbsb3k("nfa"));
}
\arguments{
\item{x}{object to be scrutinazed}
}
\value{
TRUE when the object is considered as empty FALSE if not
}
\keyword{programming}
