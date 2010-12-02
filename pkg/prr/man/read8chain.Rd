\name{read8chain}
\alias{read8chain}
\title{
gets a /chain/ from a text file
}
\description{
Produces a /chain/ from a text file. The text file must follow the
structure compatible with \code{file2list}. The first component of
the file must be the description of the chain; the remaining
components are supposed to be the nodes (see the the text file
examples).\cr \code{write8chain} onto \code{prr.chain1} can be used
to get an example of such a text file (see the examples).\cr See also
the "Details" section for further possibility.
}
\usage{
read8chain(fi)
}
\examples{
 prr3k("RESET"); # (only for R checking)
 write8chain(prr.chain1, "tutu.txt");
 print(read8chain("tutu.txt"), empha=2);
 unlink("tutu.txt");
}
\arguments{
\item{fi}{The file name to be considered.}
}
\details{
The tags used to decode the file are given by the \code{prr.tag1}
constant. Its values can be modified by the user if necessary.
}
\value{
The generated /chain/ object.
}
