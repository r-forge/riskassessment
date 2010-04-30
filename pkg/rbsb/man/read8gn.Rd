\name{read8gn}
\alias{read8gn}
\title{
(gn) produced a /gn/ from a text file
}
\description{
The text file must follow the structure compatible with
\code{file2list}. The first component of the file must be the
description of the /gn/; the remaining components are supposed to be
the nodes.
}
\usage{
read8gn(fi)
}
\arguments{
\item{fi}{The name of the file to be considered.}
}
\details{
See \code{rebastaba.etg?.txt} files to get straightforward examples.
}
\value{
The generated /gn/ object
}
