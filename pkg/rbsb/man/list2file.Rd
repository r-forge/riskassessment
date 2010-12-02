\name{list2file}
\alias{list2file}
\title{
transforms a list and writes it onto a text file
}
\description{
Transforms a list and writes it onto a text file. The reverse
operation of \code{file2list} and the linking of \code{list2char} and
\code{char2file}. See their description for complete details.
}
\usage{
list2file(lili, file, path=".", tags=rbsb.tag1, stag=c("/", ";", "/"),
	  comment="#", comments=character(0), ap=FALSE)
}
\examples{
 rbsb3k("reset");
 list2file(rbsb.lis1, "toto.txt");
 unlink("toto.txt");
 list2file(rbsb.lis1, file=rbsb.cha0); 
}
\arguments{
\item{lili}{ The list structure to be transformed and written in
\code{file}.}
\item{file}{ file to be written. According to \code{ap} when already
exist, it will be destroyed and recreated or completed. But when
equal to \code{rbsb.cha0} no file is considered but the potential
content of the file is returned as a character. }
\item{path}{ Directory containing the file.}
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
\item{ap}{Must the file be appended if it already exist?}
}
\details{
No use is made of the general constant \code{rbsb.mck}, being
reported in the called functions.\cr In any case, permission of
writting must exist in the directory of the proposed path.
}
\value{
Nothing but a file is created or modified when everything is right;
except when \code{file==rbsb.cha0}, in that case a character is
returned.
}
\keyword{IO}
