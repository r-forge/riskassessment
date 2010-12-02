\name{write8chain}
\alias{write8chain}
\title{
writes a /chain/ onto a file
}
\description{
writes a /chain/ object to a text file to be readable with
\code{read8bn}
}
\usage{
write8chain(chain, fi, ap=FALSE)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 write8chain(prr.chain1, rbsb.cha0);
}
\arguments{
\item{chain}{The /chain/ to be written.}
\item{fi}{name of the file to be written. When \code{rbsb.cha0} no
file is written but a character is returned.}
\item{ap}{Must a possible existing file be appended?}
}
\value{
According to \code{fi} (1) nothing and a file is created or modified
or (2) a character with the content of the file.
}
