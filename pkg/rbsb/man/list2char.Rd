\name{list2char}
\alias{list2char}
\title{
transforms a list into a character
}
\description{
The reverse operation of \code{char2list}. The list must be a
rsbs-list, that is compying some properties : all components of the
[sub-]lists must be either a list or a [named] character
vector/matrix/array. The number of nested list must not be greater
than the number of rows in matrix \code{tags}.\cr Every list
component must be named.\cr The idea is to get a character compatible
with \code{char2list} to produce back the object \code{lili}.\cr Some
comments are added to the content of the list by the function itself
and/or according to the whish of the user, this is why an escaping
character is asked.
}
\usage{
list2char(lili, tags=rbsb.tag1, stag=c("/", ";", "/"), comment="#",
	  comments=character(0))
}
\examples{
 rbsb3k("reset");
 list2char(rbsb.lis1);
}
\arguments{
\item{lili}{ The list structure to be transformed into a
\code{character}.}
\item{tags}{ Character matrix with two columns indicating the opening
and closing tags of the [sub]lists at different levels. Its row
numbers gives the maximum accepted number of levels. Opening tags
must be different.}
\item{stag}{ Three character strings indicating the tagging to define
the separator for each character vector \code{stag[2]} between
\code{stag[1]} and \code{stag[3]}.}
\item{comment}{At the beginning of a line, it indicates that this
line must not be considered. This function will introduce its
signature at the beginning of the file.}
\item{comments}{Comments that the user want to be added at the
beginning of the file.}
}
\details{
Also, the character strings of the structure must not comprise the
\code{rbsb.sep0} constant except when this global constant is
conveniently modified. The same for \code{rbsb.sep1}.\cr Use is made
of the general constant \code{rbsb.mck} for the checking.
}
\value{
The resulting character.
}
\keyword{IO}
