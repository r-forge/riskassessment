\name{char2nom}
\alias{char2nom}
\title{
(ba) transforms a character into a 'nom' object
}
\description{
returns a \code{/nom/} whose name is \code{x}.
}
\usage{
char2nom(x)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 print(char2nom(LETTERS));
}
\arguments{
\item{x}{character.}
}
\value{
a 'nom' object with node having the non-named variable
}
\keyword{misc}
