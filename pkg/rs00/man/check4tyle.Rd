\name{check4tyle}
\alias{check4tyle}
\title{
(00) checks the type and the length of some standard object
}
\description{
If not correct, a fatal message is issued. NA are detected and
considered as wrong.
}
\usage{
check4tyle(x, typ, len, message=NULL, fatal=TRUE, na.rm=FALSE)
}
\examples{
 check4tyle(letters, c("numeric", "character"), c(20, 30), "!OK");
}
\arguments{
\item{x}{object to be checked.}
\item{typ}{The list of correct types, among
'null",'integer','numeric','character',
'logical','list','any','function'. As understood, 'any' implies that
no check of the type is performed.}
\item{len}{If length(len)==1, the exact length to be checked, if not
must be of length for the possible range of length(x). When -1, no
check on the length is performed.\cr For data.frame, it is the number
of columns. When \code{na.rm} the check is made after removing the NA
values.}
\item{message}{ Some additional message to be issued before
stopping.}
\item{fatal}{ what to do when discovering an inconsistency ? TRUE:
this function prints the message and stops; FALSE: this function
returns the message as a }
\item{na.rm}{ Must the \code{NA} values be removed before checking?
This implemented only for the type integer, numeric, character and
logical.}
}
\details{
'integer' has not got the meaning in is.integer R standard function.
'null' must be understood as resulting TRUE with 'isempty'.
}
\value{
When the check is validated returns TRUE. If not, according to
\code{fatal} prints the message and stops or returns the message.
}
\keyword{error}
