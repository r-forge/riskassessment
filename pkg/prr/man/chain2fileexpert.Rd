\name{chain2fileexpert}
\alias{chain2fileexpert}
\title{
writes from a /chain/ a file to be completed by an expert
}
\description{
From a complete chain, write (with possibly many comments) a file to
be completed by the assessement of expert(s). Only necessary of
components of \code{@assess} of every node are proposed
}
\usage{
chain2fileexpert(chain, file, comment=2)
}
\examples{
 prr3k("RESET"); # For R checking compliance
 chain2fileexpert(prr.chain3, rbsb.cha0);
}
\arguments{
\item{chain}{The /chain/ serving as template.}
\item{file}{name of the file to be written. When \code{rbsb.cha0} no
file is written but a character is returned.}
\item{comment}{ Indicates the level of the comment to introduce in
the created file. [0] no comment, [1] just the date of the creation,
[2] as [1] plus some brief explanations,}
}
\details{
The values of the original /chain/ are recalled in the comments when
\code{comment} is greater than zero.
}
\value{
The content of the file is returned or written to the designated
file.
}
