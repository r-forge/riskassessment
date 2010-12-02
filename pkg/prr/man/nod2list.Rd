\name{nod2list}
\alias{nod2list}
\title{
transforms a /nod/ into a consistent list
}
\description{
Transforms a /nod/ into a consistent list doing the necessary
translations to make things easy for the end user.
}
\usage{
nod2list(nod)
}
\examples{
 prr3k("RESET"); # (only for R checking)
 nod2list(prr.nod3);
}
\arguments{
\item{nod}{The nod object to be transformed into a list.}
}
\details{
According to \code{prr.mck}, the argument \code{nod} is checked or
not.
}
\value{
The generated list.
}
