\name{file2list}
\alias{file2list}
\title{
reads a file and transforms it in a list (of lists) of characters
}
\description{
reads a conveniently tagged file into nested lists. It is just the
linking of the two functions \code{file2char} and \code{char2list},
see their comments for the description of the arguments.
}
\usage{
file2list(file, path="./", clean=TRUE, ended=")STOP(", comme="#",
	  skip=matrix(c(")SKIPPING(", ")READING("), 1),
	  include=c(")FILE("), tags=rbsb.tag1, sep=rbsb.sep0,
	  rsep="no_action", stag=c("/", "/"))
}
\examples{
 rbsb3k("reset"); # for R checking convenience
 sink("rbsb.list.txt")
 cat("# comments can be included as well\n")
 cat("<<A>>\n");
 cat("[[a]]/*/v 1*un deux trois\n");
 cat("[[b]]/*/v 1*2*3\n");
 cat("un uno one\n");
 cat("deux dos two\n");
 cat("trois tres three\n");
 cat("<<B>>\n");
 cat("[[a]] un deux trois\n");
 cat("# the following three are interesting\n");
 cat("[[b]] un uno one\n");
 cat(" deux dos two\n");
 cat("trois tres three\n");
 cat("<<C>> 1 2 3\n");
 sink();
 file2list("rbsb.list.txt");
 unlink("rbsb.list.txt");
}
\arguments{
\item{file}{ file to be read and transformed into a list.}
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
\item{tags}{ Character matrix with two columns indicating the opening
and closing tags of the lists at different levels. Its row numbers
gives the maximum number of levels. Opening tags must be different.}
\item{sep}{ Character sequence used to split the character vectors of
every line. Notice that \code{LF} is always considered as a
separator.}
\item{rsep}{ Indicates if repetitions of \code{sep} must be
considered as signicant or not and which null value to introduce. If
\code{no_action} then the repetitions will be ignored if not
\code{rsep} component(s) will be introduced.}
\item{stag}{ Two character strings indicating the tag to define
different \code{sep} for a given [sub]list. These two correspond to
\code{stag[c(1,3)]} of \code{list2file} function.}
}
\value{
a list [of lists [of lists [...] ] ] of character (possibly named)
vectors or matrices or arrays.
}
\keyword{IO}
