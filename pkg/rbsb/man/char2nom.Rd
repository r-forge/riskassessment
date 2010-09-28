\name{char2nom}
\alias{char2nom}
\title{
transforms a character into a 'nom' object
}
\description{
returns a \code{/nom/} whose names/variables comes from \code{nova}.
Repetitions are possible and eliminated.
}
\usage{
char2nom(nova)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 print.default(char2nom(LETTERS[1:4]));
 print(char2nom(c("C[b]", "A", "B[one]", "C[b]", "C[a]", "B[two]")));
}
\arguments{
\item{nova}{character.}
}
\details{
nodes and variables/nodes are sorted.
}
\value{
a 'nom' object with nodes possibly comprising covariables
}
\keyword{misc}
