\name{char2list}
\alias{char2list}
\title{
transforms a character into a list (of lists) of characters
}
\description{
from a conveniently tagged character vector, returns nested lists.
Most often such a character is obtained by reading a file with the
function \code{file2char}, this explains why it is convenient to
speak about 'lines' rather about 'components of the character
vector'.\cr All lists are named lists and the tags give the names of
their component. The maximum number of nested levels of lists is
given by the number of rows of the matrix \code{tags}. The
corresponding two columns give the opening and closing sequence of
the tags. Final lists contain \code{character} vectors, each
component of them being on the same line and/or on the following line
(before a new tag).\cr All tags must start at the very beginning of a
line. All separator tags must be used sticked to the list tag.
}
\usage{
char2list(chara, tags=rbsb.tag1, sep=rbsb.sep0, rsep="no_action", stag=c("/",
	  "/"))
}
\examples{
 rbsb3k("reset"); # for R checking convenience
 chara <- c("<<A>>", 
 "[[a]]/*/v 1*un deux trois", 
 "[[b]]/*/v 1*2*3", 
 "un uno one", 
 "deux dos two", 
 "trois tres three", 
 "<<B>>", 
 "[[a]] un deux trois", 
 "[[b]] un uno one", 
 " deux dos two", 
 "trois tres three", 
 "<<C>> 1 2 3");
 char2list(chara);
}
\arguments{
\item{chara}{ character vector to be transformed into a list.}
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
spaces are eliminated.\cr
}
\value{
a list [of lists [of lists [...] ] ] of character (possibly named)
vectors or matrices or arrays.
}
\keyword{IO}
