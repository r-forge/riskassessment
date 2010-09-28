\name{read8audit}
\alias{read8audit}
\title{
produced an audit /bn/ from a text file
}
\description{
(bn) The text file must follow the structure compatible with
\code{file2list}. More precisely, the result will be dealt with
list2gn and additionnal fields \$proba and \$scrores will be
integrated with new8audit
}
\usage{
read8audit(fi)
}
\arguments{
\item{fi}{The name of the file to be considered.}
}
\details{
See rebastaba.etb?.txt file to get straightforward examples.
}
\value{
The generated /bn/ object
}
