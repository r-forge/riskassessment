\name{expr3bugs}
\alias{expr3bugs}
\title{
returns the piece of Bugs code for standard distributions
}
\description{
(bn) Here according to the distribution, the parameters and the
parents the bugs piece of code is established
}
\usage{
expr3bugs(distri, bn, iid, zname, p1="", p2="", p3="", p4="")
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 cat("To be added\n");
}
\arguments{
\item{distri}{ltype of the distribution.}
\item{bn}{The Bayesian network under consideration}
\item{iid}{The node of it under consideration}
\item{zname}{The name under which it must be coded (possibly not its
name when a transformation has to be added afterwards).}
\item{p1}{ First parameter of the distribution. This expression must
already filtered with expr3cobu. }
\item{p2}{ Second parameter (if any) of the distribution.}
\item{p3}{ Third parameter (if any) of the distribution.}
\item{p4}{ Fourth parameter (if any) of the distribution.}
}
\value{
Character expression for the code
}
