\name{print8des}
\alias{print8des}
\title{
prints a des object
}
\description{
This function prints in a /des/ object.
}
\usage{
print8des(x, ..., what="ndr", empha=1)
}
\arguments{
\item{x}{The \code{des} object to print.}
\item{\dots}{Further arguments to be passed to the print function.}
\item{what}{the fields to print: \code{a}=all fields, \code{n}=name,
\code{d}=definition, \code{o}=origin, \code{t}=time, \code{r}=role,
\code{c}=comments.}
\item{empha}{Emphasize level of printing; between 0 and 10}
}
\details{
Global constant \code{rbsb.mwi} is used to justify the paragraphes.
It is the specific print for /des/ objects.
}
\value{
nothing but a print is performed
}
\keyword{classes}
