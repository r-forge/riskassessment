\name{read8bn}
\alias{read8bn}
\title{
(bn) produced a /bn/ from a text file
}
\description{
The text file must follow the structure compatible with
\code{file2list}. The first component of the file must be the
description of the bn; the remaining components are supposed to be
the nodes.\cr As much as possible, the read list is interpreted to
stick with the requirements of creating a /bn/ from it.
}
\usage{
read8bn(fi)
}
\arguments{
\item{fi}{The file name to be considered.}
}
\details{
See \code{rsbn.demo.creating2.txt} and \code{rsbn.demo.creating3.txt}
files to get a simple example. You can also use \code{write8bn} to
see the result with an existing /bn/.
}
\value{
The generated /bn/ object
}
