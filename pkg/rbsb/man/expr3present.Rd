\name{expr3present}
\alias{expr3present}
\title{
indicates inclusion of character string
}
\description{
Checks if some string(s) belong(s) to a series of strings. When
\code{exact} is FALSE, returns TRUE if the character string
\code{sch} is included at least once into the character string
\code{ch}.\cr \code{sch} can be a vector, in that case, the check is
made for each of its components. According to \code{how} the returned
value is vectorial or scalar. When \code{sch} is zero length then
\code{TRUE} is returned but if not and \code{ch} is zero length then
\code{FALSE} is returned.
}
\usage{
expr3present(sch, ch, exact=FALSE, how="a")
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 expr3present('a', 'non');
 expr3present('o', c('non', 'oui'));
 expr3present(c("o", "oui"), c('non', 'oui'));
 expr3present(c("o", "oui"), c('non', 'oui'), how="v");
 expr3present(c("A[SEX]", "A[AGE]"), c("A[AGE]", "A[SEX]"), how="a")
}
\arguments{
\item{sch}{(\code{character}) the character string(s) to be found.}
\item{ch}{(\code{character}) the character string(s) to investigate.}
\item{exact}{ When exact, one component must be strictly identical,
if not a subtring is sufficient.}
\item{how}{ Indicates what to do when \code{length(sch)>1}. The
choice are \code{'v'}: a logical vector gives back each check
independently; \code{'1'}: returns \code{TRUE} when at least one of
the component belongs to the series \code{ch} and \code{'a'} when all
components must comply to get TRUE.}
}
\value{
A logical vector with length of \code{sch}.
}
\keyword{utilities}
