\name{expr3extrait}
\alias{expr3extrait}
\title{
extracts the contents of parentheses from a character
}
\description{
returns a vector character with the contents of different
parentheses.
}
\usage{
expr3extrait(cara, opa="(", cpa=")")
}
\examples{
 rbsb3k("reset"); # for R checking convenience
 expr3extrait('avant (8h) ce n_est pas l_heure, plus tard que (9h) non plus')
}
\arguments{
\item{cara}{\code{character(1)} to be considered.}
\item{opa}{ opening tag (\code{character(1)}).}
\item{cpa}{ closing tag (\code{character(1)}).}
}
\details{
Parenthesis are defined with 'opa' and 'cpa' arguments
}
\value{
A character vector or nothing when there is no parentheses
}
\keyword{utilities}
