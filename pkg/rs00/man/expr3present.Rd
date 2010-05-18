\name{expr3present}
\alias{expr3present}
\title{
(00) does sch belongs to ch?
}
\description{
When \code{exact} is FALSE, returns TRUE if the character string sch
is included at least one time into the character string ch.\cr
}
\usage{
expr3present(sch, ch, exact=FALSE)
}
\examples{
 expr3present('a', 'non');
 expr3present('o', c('non', 'oui'));
}
\arguments{
\item{sch}{(\code{character(1)}) the character string to be found.}
\item{ch}{(\code{character}) the character string(s) to investgate.}
\item{exact}{(=FALSE) When exact, one component must be strictly
identical, if not a subtring is }
}
\value{
TRUE or FALSE
}
\keyword{utilities}
