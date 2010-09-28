\name{gn2list}
\alias{gn2list}
\title{
transforms a /gn/ object into a list
}
\description{
Just translating all slot of the /gn/ into associated components of a
list. This is the reverse function of \code{list2gn}.
}
\usage{
gn2list(gn)
}
\examples{
 g4n3k("RESET"); # For R checking compliance
 gn2list(g4n.gn3);
}
\arguments{
\item{gn}{The /gn/ to be transformed into a list.}
}
\value{
The generated list
}
