\name{sort8ion}
\alias{sort8ion}
\title{
(ba) sorts a 'ion' object
}
\description{
sorts a 'ion' object possibly eliminating the redundancies
}
\usage{
sort8ion(ion, nom, sort="n", rm.redun=TRUE)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 uu <- rbsb.nom2;
 vv <- nv2ion(0, uu);
 print(sort8ion(vv, uu));
 print(sort8ion(vv, uu, "a"));
}
\arguments{
\item{ion}{The 'ion' to be sorted.}
\item{nom}{associated 'nom' structure to perform the sorting.}
\item{sort}{(='n') the way to make the sorting 'n': according to
'nom' 'a'; according to the alphabet.}
\item{rm.redun}{(=TRUE) Must the redundancies be removed?}
}
\value{
The sorted [reduced] 'ion'
}
\keyword{misc}
