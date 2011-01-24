\name{print8daf}
\alias{print8daf}
\title{
prints a daf object
}
\description{
This function prints a /daf/ object.
}
\usage{
print8daf(x, what="nr", whi=1:10)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 print(rbsb.daf2);
}
\arguments{
\item{x}{The daf object.}
\item{what}{ which part of the description to print (see
\code{print8des}).}
\item{whi}{ which rows to print?\cr when -1: none is printed,\cr when
0: all are printed,\cr when a vector: indicates the numbers of rows
to print (default = the first ten),\cr when a value: the percentage
to be printed (50 = half of the rows) }
}
\details{
It is the specific print function for /daf/ objects.
}
\value{
returns nothing but a printing is performed
}
\keyword{print}
