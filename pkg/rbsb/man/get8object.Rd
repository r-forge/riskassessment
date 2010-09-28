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
\item{whi}{The designation of the object. The different possibilities
are given by the value of \code{whi[2]}. When "o", the object is
returned as it is (when it does not exist, NULL is returned). When
"c", must be a character. When "n", must be a numeric. When "d", must
be a data.frame. When "m", must be a matrix. When "a", must be an
array. When "f", must be a function. When "t1", must be a text file
to be read with \code{read.table}. When "t2", must be a text file to
be read with \code{read.csv}. When "t3", must be a text file to be
read with \code{read.csv2}. (when reading a file, the corresponding
function is called with \code{,header=TRUE,comment.char="#"}.) }
}
\details{
This is similar in spirit to the function \code{get8daf} but much
less specialized.
}
\value{
The object. Except when \code{whi[2]=="o"} an error is issued if the
object does not exist or is not from the expected class.
}
\keyword{misc}
