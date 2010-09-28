\name{sort8ion}
\alias{sort8ion}
\title{
sorts a 'ion' object
}
\description{
(ba) sorts a 'ion' object possibly eliminating the redundancies
}
\usage{
sort8ion(ion, nom, sort="n", rm.redun=TRUE)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 uu <- rbsb.nom2;
 vv <- nv2ion(0, uu);
 print(sort8ion(vv, uu));
 print(sort8ion(vv, uu, "a"));
}
\arguments{
\item{ion}{The 'ion' to be sorted.}
\item{nom}{associated 'nom' structure to perform the sorting.}
\item{sort}{ the way to make the sorting 'n': according to 'nom' 'a';
according to the alphabet.}
\item{rm.redun}{ Must the redundancies be removed?}
}
\details{
The algorithm does note take care of \code{rbsb.who}, changing it
implies changing this algorithm...
}
\value{
The sorted [reduced] 'ion'
}
\keyword{misc}
