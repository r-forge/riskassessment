\name{list2file}
\alias{list2file}
\title{
(00) transforms a list and write it onto a character file
}
\description{
The reverse operation of \code{list2file}. The list must be a
rsbs-list, that is to comply some properties : all components of the
[sub-]lists must be either a list or a [named] character
vector/matrix/array. The number of nested list must not be greater
than the number of rows in matrix \code{tags}.\cr Every list
component must be named.\cr The idea is to get a file readable by
\code{file2list} to produce back the object \code{lili}.
}
\usage{
list2file(lili, file, path="./", tags=matrix(c("<<", ">>", "[[", "]]", "((",
	  "))"), ncol=2, byrow=TRUE), stag=c("/", "/"), comment="#",
	  comments=character(0))
}
\examples{
 rs003k("reset");
 uu <- list(A=1:3, 
 B=matrix(letters[1:20], nrow=5, dimnames=list(1:5, c("on", "tw", "th", "fo"))), 
 C=list(a=1:3, b=letters, c=list(final=array(1:8, c(2, 2, 2)))));
 list2file(uu, "toto.txt");
 unlink("toto.txt");
}
\arguments{
\item{lili}{ The list structure to be transformed and written in
\code{file}.}
\item{file}{ file to be written. If it already exist, it will be
destroyed and recreated.}
\item{path}{ Directory containing the file.}
\item{tags}{ Character matrix with two columns indicating the opening
and closing tags of the [sub]lists at different levels. Its row
numbers gives the maximum accepted number of levels. Opening tags
must be different.}
\item{stag}{ Two character strings indicating the tagging to define
the separator for each character vector.}
\item{comment}{At the beginning of a line, it indicates that this
line must not be considered. This function will introduce its
signature at the beginning of the file.}
\item{comments}{Comments that the user want to be added at the
beginning of the file.}
}
\details{
Also, the character strings of the structure must not comprise the
\code{rbsb.sep0} constant in case, this global constant must be
conveniently modified. The same for \code{rbsb.sep1}.\cr Use is made
of the general constant \code{rbsb.mck}.
}
\value{
Nothing but a file is created when everything is right.
}
\keyword{IO}
