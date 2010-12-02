\name{mplot8nod}
\alias{mplot8nod}
\title{
simulates and plots densities of a /nod/ varying its parameters
}
\description{
Plots a matrix of plots containing each a /nod/ distribution. This is
defined through a \code{nod} object whose \code{@assess} slot is
modifyed with the values indicated in the three \code{x,cV,cU}
arguments.\cr The plots varies in row-column in one of the three way:
(cv,cu\eqn{|}x fixed), (x,cu\eqn{|}cv fixed) or (x,cv\eqn{|}cu fixed)
indicated by the fact fact that one of the \code{x}, \code{cv} or
\code{cu} argument is provided with only one value.
}
\usage{
mplot8nod(nod, nbs=1000, x=NULL, cV=seq(5, 95, length=4), cU=seq(5, 95,
	  length=4), ...)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 mplot8nod(prr.nod2);
}
\arguments{
\item{nod}{The node defining the distribution except for
\code{@assess} values.}
\item{nbs}{Number of values to simulate to numerically obtain a
density graph.}
\item{x}{central value(s) for the distribution. When \code{NULL},
will be put to \code{as.numeric(nod@assess@x)}.}
\item{cV}{variability value(s) for the distribution.}
\item{cU}{uncertainty value(s) for the distribution.}
\item{\dots}{To be passed to the \code{plot8distri} function.}
}
\details{
The matrix of plots is done by successive call to \code{plot8nod}.
The adjustment of \code{par(mfrow)} is done within the function. To
generate a graph file, \code{pdf} or a similar function must be
called beforehand, as well as \code{dev.off} afterwards.
}
\value{
Nothing but a matrix of plots is issued.
}
