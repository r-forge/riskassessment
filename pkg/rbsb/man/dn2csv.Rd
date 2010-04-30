\name{dn2csv}
\alias{dn2csv}
\title{
(dn) creates a csv file containing dn@df
}
\description{
Produces a new text file named \code{fi} with the slot \code{dn@df}.
}
\usage{
dn2csv(dn, fi)
}
\examples{
 rsdn3k("RESET"); # to comply R checking
 dn2csv(rbsb.dn2, "toto.txt");
 read.csv("toto.txt", row.names=1, check.names=FALSE);
 unlink("toto.txt");
}
\arguments{
\item{dn}{The /dn/.}
\item{fi}{The file name..}
}
\value{
Nothing, the file is created. It can be read back with
\code{read.csv(fi,row.names=1,check.names=FALSE)}.
}
\keyword{IO}
