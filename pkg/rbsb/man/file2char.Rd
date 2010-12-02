\name{file2char}
\alias{file2char}
\title{
reads a file and transforms it in a single character
}
\description{
reads a conveniently tagged file to produce a vector of character
where all non used lines are eliminated. Each component of the
resulting vector is associated to an original line.
}
\usage{
file2char(file, path="./", clean=TRUE, ended=")STOP(", comme="#",
	  skip=matrix(c(")START_SKIPPING(", ")END_SKIPPING(", "/*",
	  "*/"), ncol=2, byrow=TRUE), include=c(")FILE("))
}
\examples{
 rbsb3k("reset"); # for R checking convenience
 sink("rbsb.char.txt")
 cat("# comments can be included as well\n")
 cat(" something\n");
 cat("/* skipping from here\n");
 cat("blablabla\n");
 cat(" */ until here (this line is ALSO eliminated\n");
 cat(" interesting:\n");
 cat("un dos tres\n");
 cat(")STOP(\n");
 cat(" It is finished!\n");
 cat(" Don't insist!\n");
 sink();
 file2char("rbsb.char.txt");
 unlink("rbsb.char.txt");
}
\arguments{
\item{file}{ file which have to be read and transformed into a list.}
\item{path}{ Directory containing the file.}
\item{clean}{Indicates if starting and ending spaces must be
eliminated at first.}
\item{ended}{ To indicate the line from which to stop the reading.}
\item{comme}{At the beginning of a line, it indicates that this line
must not be considered. More than one commenting character can be
considered when it is a vector. For instance \code{c("#","%
")} means that lines starting with an hash or a percent are comment
lines.}
\item{skip}{ To indicate set(s) of lines to be skipped. Must be a
character matrix where the two columns correspond respectively to the
opening and closing tags, and where each row is associate to a couple
of tags. Tags are considered successively following the order of
these matrix rows.}
\item{include}{ Tags to indicate a file (including possible path) by
a \code{character(1)} to include at this point its contents as a text
file with the same tags specifications. Including files can be
recursive. }
}
\details{
Performed actions are : (i) eliminating commented lines, (ii)
eliminating lines after a 'stop', (iii) including indicated files,
(iv) skipping sequences of lines and (v) cleaning the line, i.e.
removing starting and ending spaces.\cr All tags are supposed to be
in the first position of the line after cleaning when asked.
}
\value{
a character of length, the number of retained lines.
}
\keyword{IO}
