\name{expr3extrait}
\alias{expr3extrait}
\title{
(00) extracts the contents of parentheses from a character
}
\description{
returns a vector character with the contents of different
parentheses.
}
\usage{
expr3extrait(cara, opa="(", cpa=")")
}
\examples{
 expr3extrait('avant (8h) ce n_est pas l_heure, plus tard que (9h) non plus')
}
\arguments{
\item{cara}{Character to be considered.}
\item{opa}{ opening tag.}
\item{cpa}{ closing tag.}
}
\details{
Parenthesis are defined with 'opa' and 'cpa' arguments
}
\value{
A character vector or nothing when there is no parentheses
}
\keyword{utilities}
