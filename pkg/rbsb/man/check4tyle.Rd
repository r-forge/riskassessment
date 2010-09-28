\name{check4tyle}
\alias{check4tyle}
\title{
checks the type and the length of some standard object
}
\description{
If not correct, a fatal message is issued. NA are detected and
considered as wrong.
}
\usage{
check4tyle(x, typ, len, con=rbsb.num0, message=NULL, fatal=TRUE, na.rm=FALSE)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 check4tyle(letters, c("numeric", "character"), c(20, 30), message="!OK");
 check4tyle(structure("A", .Names="a"), "ncharacter", -1, LETTERS);
 check4tyle("A", "ncharacter", -1, message="BAD", fatal=FALSE);
 check4tyle(structure("A", .Names="a"), "character", -1, letters, "bad", fatal=FALSE);
}
\arguments{
\item{x}{object to be checked.}
\item{typ}{The list of correct types, among 'null', 'integer',
'numeric', 'character', 'logical', 'list', 'any', 'function',
'data.frame', 'matrix'. Also are 'nlogical', 'ninteger', 'nnumeric'
and 'ncharacter' for named structures. As understood, 'any' implies
that no check of the type is performed.}
\item{len}{If length(len)==1, the exact length to be checked, if not
must be of length for the possible range of length(x). When -1, no
check on the length is performed.\cr For data.frame, it is the number
of columns. When \code{na.rm} the check is made after removing the NA
values.}
\item{con}{ If length(con)\eqn{>}0, some check about the content of
\code{x} is done for some of the types. More precisely for (integer,
numeric): all values must belong to the interval
\code{[con[1],con[2]]} and for (character), the set of possible
\code{character(1)} is described by \code{con}.}
\item{message}{ Some additional message to be issued before
stopping.}
\item{fatal}{ what to do when discovering an inconsistency ? TRUE:
this function prints the message and stops; FALSE: this function
returns the message as a }
\item{na.rm}{ Must the \code{NA} values be removed before checking?
This is implemented only for the types integer, numeric, character
and logical.}
}
\details{
'integer' has not got the meaning in is.integer R standard function.
'null' must be understood as resulting TRUE with function
\code{isempty}.\cr 'Named character' are not considered as been
'character'.
}
\value{
a character with the message(s), when everything is OK returns a
\code{character(0)}. But when \code{fatal} if not OK prints the
message and stops.
}
\keyword{error}
