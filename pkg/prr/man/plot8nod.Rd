\name{plot8nod}
\alias{plot8nod}
\title{
plots the density of an ancestor /nod/.
}
\description{
Plots the numerically approximated distribution of a /nod/ not
depending on parents through a call to \code{plot(density())}. The
approximation is made with the help of \code{nbs} simulations.
}
\usage{
plot8nod(nod, nbs=1000, tit=NULL, ...)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 plot8nod(prr.nod1);
 plot8nod(prr.nod2);
}
\arguments{
\item{nod}{The /nod/ object of which the repartition is looked for}
\item{nbs}{Number of simulations to perform to approximate the
density.}
\item{tit}{When \code{NULL}, a standard title is used.}
\item{\dots}{To be passed to the standard 'plot' function.}
}
\details{
The central value \code{nod@assess@x} is drawn, as are the limits of
the support.
}
\value{
Nothing but a plot is issued.
}
