\name{get8listcomp}
\alias{get8listcomp}
\title{
(00) returns components from a list structure
}
\description{
Returns components from a list structure as a one level list. The
list \code{lili} must have been explored with \code{explore8list} and
the branch(es) to return are indicated through their lines in the
generated table (\code{tata}).
}
\usage{
get8listcomp(lili, tata)
}
\examples{
 rs003k("reset");
 uu <- list(A=1:3, 
 B=matrix(letters[1:20], nrow=5, dimnames=list(1:5, c("on", "tw", "th", "fo"))), 
 C=list(a=1:3, b=letters, c=list(final=array(1:8, c(2, 2, 2)))));
 vv <- explore8list(uu);
 get8listcomp(uu, vv[7, ])[[1]];
 uu <- list(1:3, 
 matrix(letters[1:20], nrow=5, dimnames=list(1:5, c("on", "tw", "th", "fo"))), 
 list(a=1:3, b=letters, c=list(final=array(1:8, c(2, 2, 2)))));
 vv <- explore8list(uu);
 get8listcomp(uu, vv[7, ])[[1]];
}
\arguments{
\item{lili}{ The list structure components of which have to be
extracted.}
\item{tata}{ The lines of the table provided by \code{explore8list}.}
}
\details{
Names of the produced list are the stacked names of the initial list
\code{lili}.
}
\value{
The resulting list with as many component as indicated rows.
}
\keyword{IO}
