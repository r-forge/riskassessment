\name{check4tyle}
\alias{check4tyle}
\title{
(f0) checks the type and the length of some standard object
}
\description{
If not correct, a fatal message is issued
}
\usage{
check4tyle(x, typ, len, message=NULL)
}
\examples{
 check4tyle(letters, c("numeric", "character"), c(20, 30), "!OK");
}
\arguments{
\item{x}{object to be checked.}
\item{typ}{The list of correct types, among
'null",'integer','numeric','character','list','any','function'. As
understood, 'any' implies that no check of the type is performed.}
\item{len}{If length(len)==1, the exact length to be checked, if not
must be of length for the possible range of length(x). When -1, no
check on the length is performed.\cr For data.frame, it is the number
of columns.}
\item{message}{ Some additional message to be issued before
stopping.}
}
\details{
'integer' has not got the meaning in is.integer R standard function.
'null' must be understood as resulting TRUE with 'isempty'.
}
\value{
Nothing but a fatal error is issued when the check fails.
}
\keyword{error}
