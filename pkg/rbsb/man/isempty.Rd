\name{isempty}
\alias{isempty}
\title{
tests the nullness of objects
}
\description{
Returns TRUE is the structure is empty.\cr This trick was proposed
because the \code{NULL} cannot replace any kind of objects.
Generally, constant finishing with \code{0} like \code{rbsb.lis0},
\code{rbsb.dfr0}... are null objects.\cr Notice that
\code{isempty("")} is FALSE.
}
\usage{
isempty(x)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 isempty(numeric(0));
 isempty(NULL);
 isempty(rbsb.fau0);
 isempty("");
}
\arguments{
\item{x}{object to be scrutinazed}
}
\details{
see the code to know the list.
}
\value{
TRUE when the object is considered as empty FALSE if not
}
\keyword{programming}
