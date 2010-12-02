\name{get8nam}
\alias{get8nam}
\title{
returns the name of a node from the name of a subnode
}
\description{
Mainly of use for \code{look4simu}.
}
\usage{
get8nam(nam)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 get8nam("foo.CV");
}
\arguments{
\item{nam}{\code{character(1)} of the subnode name.}
}
\value{
A list with two components: 

\item{\$nam}{the node name.}
\item{\$quoi}{the type of the subnod \code{m|CV|X}.}
}
