\name{write8gn}
\alias{write8gn}
\title{
writes a /gn/ to file
}
\description{
writes a /gn/ objects to a file to be readable with \code{read8gn}
}
\usage{
write8gn(gn, fi=rbsb.fou, ap=FALSE)
}
\examples{
 g4n3k("RESET"); # For R checking compliance
 write8gn(g4n.gn3);
 readLines(rbsb.fou);
 unlink(rbsb.fou);
}
\arguments{
\item{gn}{The /gn/ to be written.}
\item{fi}{name of the file to be written. When \code{rbsb.cha0} no
file is written but a charecter is returned.}
\item{ap}{Must an existing file be appended?}
}
\value{
nothing but a file is created or modified
}
