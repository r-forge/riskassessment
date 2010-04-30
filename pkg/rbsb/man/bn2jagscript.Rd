\name{bn2jagscript}
\alias{bn2jagscript}
\title{
(bn) constructs a Jags script from a bn
}
\description{
/ not available for the moment / exploring all nodes of a bn,
produces a set of lines which can serve as starting script for jags.
No data is considered; selected nodes are monitored; iteration
numbers are indicated
}
\usage{
bn2jagscript(bn, modna, codna, quoi=bn@nom, iter=c(1000, 10000, 10))
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 cat("To be added\n");
}
\arguments{
\item{bn}{bn to translate}
\item{modna}{model name: i.e. the file containing the model
definition.}
\item{codna}{coda root name: i.e. the stem for the file where Jags
will store the simulation results.}
\item{quoi}{(=bn@nom) set of nodes to monitor.}
\item{iter}{(=c(1000,10000,10) respectively the number of iterations
for the burnin, the number of iterations for the monitoring and their
thinning value.}
}
\details{
Completely specific to Jags dialect
}
\value{
A character vector, a component a line.
}
