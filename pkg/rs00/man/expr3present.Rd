\name{expr3present}
\alias{expr3present}
\title{
(f0) (sch belongs to ch)
}
\description{
When \code{vect} is FALSE, returns TRUE if the character string sch
is included at least one time into the character string ch.\cr When
\code{vect} is TRUE, returns TRUE when one of the component of the
vector \code{ch} is identical to \code{sch};
}
\usage{
expr3present(sch, ch, exact=FALSE)
}
\examples{
 expr3present('a', 'non');
 expr3present('n', 'non');
}
\arguments{
\item{sch}{the character string to be found.}
\item{ch}{the character string where to look for}
\item{exact}{(=FALSE) When exact, one component must be strictly
identical, if not a subtring is }
}
\value{
TRUE or FALSE
}
\keyword{utilities}
