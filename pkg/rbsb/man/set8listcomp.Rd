\name{set8listcomp}
\alias{set8listcomp}
\title{
modifies one component from a list structure
}
\description{
Replaces one component from a list structure which must not be a list
(neither before or after the replacement). The list \code{lili} must
have been explored with \code{explore8list} and the branch to modify
is indicated through its line in the generated table by \code{tata}.
}
\usage{
set8listcomp(x, lili, tata)
}
\examples{
 rbsb3k("reset");
 uu <- list(A=1:3, 
 B=matrix(letters[1:20], nrow=5, dimnames=list(1:5, c("on", "tw", "th", "fo"))), 
 C=list(a=1:3, b=letters, c=list(final=array(1:8, c(2, 2, 2)))));
 vv <- explore8list(uu);
 set8listcomp(LETTERS, uu, vv[7, ]);
}
\arguments{
\item{x}{ The object to be inserted in place of the former one.}
\item{lili}{ The list structure a component of which is to be
modified.}
\item{tata}{ The line (not the line number) of the table provided by
\code{explore8list} indicating the component to replace.}
}
\value{
The new resulting list
}
\keyword{IO}
