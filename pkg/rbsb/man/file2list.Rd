\name{file2list}
\alias{file2list}
\title{
(00) reads a file and transforms it in a list (of lists) of
characters
}
\description{
reads a conveniently tagged file into nested lists. All lists are
named lists and the tags give the names of their component. The
maximum number of nested levels of lists is given by the number of
rows of the matrix \code{tags}. The corresponding two columns give
the opening and closing sequence of the tags. Final lists contain
\code{character} vectors, each component of them being on the same
line and/or on the following line (before a new tag).\cr All tags
must start at the very beginning of a line. All separator tags must
be used sticked to the list tag.
}
\usage{
file2list(file, path="./", tags=matrix(c("<<", ">>", "[[", "]]", "((", "))"),
	  ncol=2, byrow=TRUE), sep=rbsb.sep0, rsep="no_action",
	  stag=c("/", "/"), comment="#", end="(STOP)",
	  beg.skip="(START_SKIPPING)", end.skip="(END_SKIPPING)")
}
\examples{
 rs003k("reset"); # for R checking convenience
 sink("list.txt")
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
 file2list("list.txt");
 unlink("list.txt");
}
\arguments{
\item{file}{ file which have to be transformed into a list.}
\item{path}{ Directory containing the file.}
\item{tags}{ Character matrix with two columns indicating the opening
and closing tags of the lists at different levels. Its row numbers
gives the maximum number of levels. Opening tags must be different.}
\item{sep}{ Character sequence used to split the character vectors on
every line. That is \code{LF} is always considered as a separator.}
\item{rsep}{ Indicates if repetitions of \code{sep} must be
considered as signicant or not. If \code{no_action} then their
repetitions will be ignored if not \code{character(0)} components
will be introduced.}
\item{stag}{ Two character strings indicating the tag to define
different \code{sep} for a given [sub]list.}
\item{comment}{At the beginning of a line, it indicates that this
line must not be considered.}
\item{end}{ To indicate the line to stop the reading (must be in
first position).}
\item{beg.skip}{ To indicate the line from which skipping the reading
(must be in first position).}
\item{end.skip}{ To indicate the line after which skipping the
reading should be stopped (must be in first position).}
}
\details{
It is compulsory to tag each level of the lists, this implies that
when starting a new list, a character vma (see \code{char2vma} for
the details: vma means vector or matrix or array) is provided meaning
that this is the final level for this branch, or a new sublist is
started with the corresponding tag, or a new component of the list is
given at a level less or equal to the present.\cr Separator between
different character component is given by the \code{sep} argument or
indicated by the separator tag (\code{stag}), in the last case, it
can be different from a leaf list to another.\cr Be aware that before
reading a line for a character translation, all starting and ending
spaces are eliminated.\cr Lines starting with \code{comment} are
escaped to give the possibility to the user to introduce comments for
humans within the file.\cr A line starting with \code{end} indicates
that the file must not be read further. A line starting with
\code{beg.skip} indicates that from there the lines must not be
considered before a line starting with \code{end.skip}.\cr The
tagging of a new [sub]list component can comprise one, two or three
indications. These must be given without space between them (but this
is possible within some) and they must be separated from possible
character information by a least one space (which will be
eliminated).\cr Separators of the structure vma (\code{xsep} of
function \code{char2vma} is imposed to be \code{rbsb.sep1}.
}
\value{
a list [of lists [of lists [...] ] ] of character (possibly named)
vectors
}
\keyword{IO}
