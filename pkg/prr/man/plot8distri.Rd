\name{plot8distri}
\alias{plot8distri}
\title{
plots the density of a /distri/.
}
\description{
Plots the numerically approximated distribution of a /distri/
distribution defined through a call to \code{plot(density())}. The
approximation is made with the help of \code{nbs} simulations
repeating the drawings in \code{rdistri}.
}
\usage{
plot8distri(distri, nbs=100, tit=deparse(substitute(distri)), ...)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 plot8distri(prr.distri1);
 plot8distri(prr.distri2);
}
\arguments{
\item{distri}{The distri object to get the repartition.}
\item{nbs}{Number of simulations to perform to approximate the
density.}
\item{tit}{Title for the graph.}
\item{\dots}{To be passed to the standard 'plot' function.}
}
\value{
Nothing but a plot is issued.
}
