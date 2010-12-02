\name{get8object}
\alias{get8object}
\title{
returns an object from a character(2) coding
}
\description{
Returns the object associated to two character strings. \code{whi[1]}
designates a global variate and \code{whi[2]} indicates the way to
interpret it.
}
\usage{
get8object(whi)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 get8object(c("rbsb.cha0", "o"));
 #will rise an error
 #get8object(c("rbsb.cha0", "c"));
 uu <- "Bonjour";
 get8object(c("uu", "c"));
 get8object(c("rbsb.dfr1", "d"));
 get8object(c("rbsb.fun0", "f"));
}
\arguments{
\item{whi}{The designation of the object.\cr\cr \code{whi[1]} is the
name of the object and \code{whi[2]} indicates the way interpret it.
The different possibilities are: \cr When "o", the object is returned
as it is (when it does not exist, NULL is returned). \cr When "c",
must be a character. \cr When "n", must be a numeric. \cr When "d",
must be a data.frame. \cr When "m", must be a matrix. \cr When "a",
must be an array. \cr When "f", must be a function. \cr When "t1",
must be a text file to be read with \code{read.table}. \cr When "t2",
must be a text file to be read with \code{read.csv}. \cr When "t3",
must be a text file to be read with \code{read.csv2}. \cr (when
reading a file, the corresponding function is called with
\code{header=TRUE,comment.char="#"}.) }
}
\details{
Very similar in spirit to the function \code{get8daf} but much less
specialized.
}
\value{
The object. Except when \code{whi[2]=="o"} an error is issued if the
object does not exist or is not from the expected class.
}
\keyword{misc}
