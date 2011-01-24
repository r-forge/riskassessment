\name{read8list}
\alias{read8list}
\title{
reads a file and transforms it in a list (of lists) of characters
}
\description{
This is a slight generalization of \code{file2list} in the sense that
the component of the list have not to be given in the file but are
determined by the function from an additionnal line (everywhere in
the file) tagged with the second argument starting at the first
character. This line contains the list of bullets, giving then their
number. Notice that all proposed bullets must be different from
\code{rbsb.l00} which can be modified if of use.\cr It is worth
explaining why such a function was written. The obvious one is that
not always names for the components of the list are natural. In
future version, it could be good to mixed with and without names.
Another reason is that the contraint of 'value only at the final
level' is not more natural. \code{read8list} adds itself 'null'
components (named \code{rbsb.l00}) to allow it. Finally, levels can
be skipped and missing levels are introduced.\cr Nevertheless,
everything has to be paid: now the constraint is that every component
must have a non empty content.
}
\usage{
read8list(file, path="./", bullet=")BULLETS(", clean=TRUE, ended=")STOP(",
	  comme="#", skip=matrix(c(")SKIPPING(", ")READING("), 1),
	  include=c(")FILE("), tags=rbsb.tag1, sep=rbsb.sep0,
	  rsep="no_action", stag=c("/", "/"))
}
\examples{
 rbsb3k("reset"); # for R checking convenience
 sink("rbsb.list.txt")
 cat(")BULLETS( * + -\n")
 cat("* FIRST LEVEL 1\n");
 cat(" + Second Level 1\n");
 cat(" - third level 1\n");
 cat(" - third level 2\n");
 cat(" + Second Level 2\n");
 cat(" + Second Level 3\n");
 cat("* FIRST LEVEL 2\n");
 cat(" + Second Level 1\n");
 cat(" + Second Level 2\n");
 cat(" - third level 1\n");
 cat(" - third level 2\n");
 cat("* FIRST LEVEL 3\n");
 sink();
 read8list("rbsb.list.txt");
 unlink("rbsb.list.txt");
 unlink(rbsb.fin);
}
\arguments{
\item{file}{ file to be read and transformed into a list.}
\item{bullet}{Indicates the bullets to be considered. This tags must
be at the very beginning of a line. But the bullets can be preceeded
by spaces.}
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
\details{
An intermediary file is generated (and not deleted) to be read by
\code{file2list} function; its names is given by the global constant
\code{rbsb.fin}.\cr The numbering of the each level of the list is
determined by cycling the global constant \code{rbsb.tly}.\cr Only
the first three arguments plus the tags are used by this function,
all others are directly passed to \code{file2list}.\cr For the
moment, the numbering of the different levels does not take into
account the level hierarchy, this must be offered in next versions of
this function.\cr
}
\value{
a list [of lists [of lists [...] ] ] of character (possibly named)
vectors or matrices or arrays.
}
\keyword{IO}
