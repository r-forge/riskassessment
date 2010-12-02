\name{look4simu}
\alias{look4simu}
\title{
makes some standard statistical investigations about a simulated
/chain/
}
\description{
From a simulation made about a /chain/ stored as a \code{list} as
done by the function \code{simu8chain} returns / prints / plots
standards results for continuous variables.
}
\usage{
look4simu(simu, what="ub", how="SP", who="A", selec=NULL)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 simul <- simu8chain(prr.chain3);
 look4simu(simul); 
}
\arguments{
\item{simu}{\code{list} resulting from a \code{simu8chain} call. The
two expected components are \code{$df} and \code{$rr}.}
\item{what}{ Indicates which kind of statistics to investigate:
\code{u} for univariate, \code{b} for bivariate.}
\item{how}{ Indicates what to do: \code{s} to return the statistics,
\code{S} to print the statistics, \code{p} or \code{P} to make
standard plots.}
\item{who}{ Which kind of subnode must be handled: \code{A} for All,
\code{X} for X, \code{m} for m and \code{C} for CV.}
\item{selec}{ Vector containing the names of the node to include,
\code{NULL} means all nodes.}
}
\details{
(see the code.)
}
\value{
According of \code{how}, only printing and plots and/or statistics
}
