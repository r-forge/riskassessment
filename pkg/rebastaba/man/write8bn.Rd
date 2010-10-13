\name{write8bn}
\alias{write8bn}
\title{
writes a /bn/ to a file
}
\description{
(bn) writes a /n/ objects to a file to be readable with
\code{read8bn}
}
\usage{
write8bn(bn, fi=rbsb.fou, ap=FALSE)
}
\examples{
 rebastaba3k("RESET"); # For R checking compliance
 write8bn(rebastaba.bn3);
 readLines(rbsb.fou);
 unlink(rbsb.fou);
}
\arguments{
\item{bn}{The /bn/ to be written.}
\item{fi}{name of the file to be written. When \code{rbsb.cha0} no
file is written but a charecter is returned.}
\item{ap}{Must an existing file be appended?}
}
\value{
nothing but a file is created or modified
}
