\name{print8gn}
\alias{print8gn}
\title{
(gn) prints an object gn
}
\description{
Prints a gn object with more or less details according to the
\code{quoi} argument.
}
\usage{
print8gn(x, ..., quoi="snrt")
}
\arguments{
\item{x}{the gn object to}
\item{\dots}{Further arguments to be passed to the standard print
function.}
\item{quoi}{(="snrt") character string giving the component to
print.\cr "t": the title,\cr "s": the size,\cr "d": the
description,\cr "n": the node names,\cr "r": the arcs,\cr "p": the
positions and the view,\cr "a": everything.}
}
\value{
returns nothing but a printing is performed
}
\keyword{print}
