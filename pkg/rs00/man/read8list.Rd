\name{read8list}
\alias{read8list}
\title{
(f0) read a list of lists of characters from a file
}
\description{
read from 'file' a list of sublists, each being a character vector.
The first level components of the list are detected from the opening
and closing brackets provided by 'lpa', each of these component is a
also a list (that is one of the sublists). The names between the
brackets 'lpa' provides names for the list components. The components
of the sublist are named or unamed character vectors. They also have
names which are given by the character before 'unique' or 'several'
indicators.\cr What is after these indicators is got has a unique or
several pieces of characters. The way it is done is indicating in the
DETAILS section.\cr Empty list are not accepted, a fatal error is
issued.
}
\usage{
read8list(file, path="./", unique="=", several=":", lpa=c("<<", ">>"),
	  end="(STOP)", beg.skip="(START_SKIPPING)",
	  end.skip="(END_SKIPPING)")
}
\examples{
 sink("list.txt")
 cat("# comments can be included as well\n")
 cat("<<A>>\n");
 cat("a(1)= un deux trois\n");
 cat("b(1)= un uno one\n");
 cat("b(2)= deux dos two\n");
 cat("b(3)= trois tres three\n");
 cat("<<B>>\n");
 cat("a: un deux trois\n");
 cat("# the following three are interesting\n");
 cat("b(/): un / uno / one\n");
 cat("b: deux dos two\n");
 cat("b: trois tres three\n");
 cat("<<C>>\n");
 cat("a: un deux trois\n");
 cat("a= un uno one\n");
 cat("a= deux dos two\n");
 cat("a= trois tres three\n");
 sink();
 read8list("list.txt");
 unlink("list.txt");
}
\arguments{
\item{file}{ file which have to be interpreted.}
\item{path}{ Directory containing the file.}
\item{unique}{ 'Chararcter indication' to distinguish the component
names (on the right) and the contents (on the left) for the
characters.}
\item{several}{ 'List indication' to distinguish the component names
(on the right) and the series of values (on the left) for the list of
characters.}
\item{lpa}{ Parentheses to use to detect the component of the list.
They cannot be ''.}
\item{end}{") The line to stop the reading (must be in first
position).}
\item{beg.skip}{") The line from which skipping the reading (must be
in first position).}
\item{end.skip}{") The line after which strop skipping the reading
(must be in first position).}
}
\details{
Lines starting with the name of a sublist can be more than one in
that case a concatenation is performed. The difference between
'unique' and 'several' lines is that you can afford no more than one
new element with 'unique' and as many you want with 'several'. The
counter part is that the element can be named with unique and not
with 'several'.\cr The names of the elements are given between
parenthesis between the name of the sublist and the 'unique'
indicator. See the EXAMPLE section for more insights.\cr Starting and
ending spaces are always eliminated with 'unique' and 'several'
lines. Then the character string is returned as one element of the
character vector in case of 'unique'. In case of 'several', it is
split according to the character string which is between parenthesis
(with ' ' as default) before 'several'. See the example section.
}
\value{
a list of lists of character (possibly named) vectors
}
\keyword{IO}
