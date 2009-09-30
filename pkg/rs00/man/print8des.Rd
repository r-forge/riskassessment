\name{print8des}
\alias{print8des}
\title{
(f3) prints a des object
}
\description{
This function prints in a interpreted way a /des/ object.
}
\usage{
print8des(x, ..., quoi="ndr", empha=1)
}
\arguments{
\item{x}{The ds object to print.}
\item{\dots}{Further arguments to be passed to the print function.}
\item{quoi}{(="dr") the fields to print: a=all fields, n=name,
d=definition, o=origin, t=time, r=role, c=comments.}
\item{empha}{(=1) Emphasize level of printing; between -2 and 5}
}
\details{
Global constant rbsb3k("mwi") is used to justify the paragraphes. It
is the generic print for /des/ objects.
}
\value{
nothing but a print is performed
}
\keyword{classes}
