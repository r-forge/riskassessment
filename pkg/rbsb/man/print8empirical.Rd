\name{print8empirical}
\alias{print8empirical}
\title{
prints an /empirical/
}
\description{
This function prints in an /empirical/ object.
}
\usage{
print8empirical(x, ..., what="rswc", empha=0)
}
\arguments{
\item{x}{The \code{empirical} object to print.}
\item{\dots}{Further arguments to be passed to the print function.}
\item{what}{the fields to print: \code{a}=all fields,
\code{d}=description, \code{s}=support, \code{r}=repartition,
\code{w}=discrete part, \code{c}=continuous part.}
\item{empha}{Emphasize level of printing; between 0 and 10}
}
\details{
Global constant \code{rbsb.mwi} is used to justify the paragraphes.
It is the specific print for /empirical/ objects.
}
\value{
nothing but a print is performed
}
\keyword{classes}
