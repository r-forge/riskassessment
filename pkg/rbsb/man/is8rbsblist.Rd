\name{is8rbsblist}
\alias{is8rbsblist}
\title{
(00) checks whether a list is rbsb-compatible list or not
}
\description{
A rbsb-list is a list with 

\describe{
\item{i}{All components and sub-components being named.}
\item{ii}{All components and sub-components being either a list or a
character vector/matrix/array. Vma components are called leafs of the
rbsb-list.}}
}
\usage{
is8rbsblist(lili)
}
\examples{
 rs003k("reset");
 uu <- list(A=1:3, 
 B=matrix(letters[1:20], nrow=5, dimnames=list(1:5, c("on", "tw", "th", "fo"))), 
 C=list(a=1:3, b=letters, c=list(final=array(1:8, c(2, 2, 2)))));
 is8rbsblist(uu);
 is8rbsblist(list(uu));
 print(rbsb.is8rbsblist, quoi="c");
 is8rbsblist(list(uu, nu=NULL));
 print(rbsb.is8rbsblist, quoi="c");
}
\arguments{
\item{lili}{ The list structure to be checked.}
}
\value{
TRUE/FALSE according to the case. Also the global variable
\code{rbsb.is8rbsblist} is modified to give the result by a
\code{des} object.
}
\keyword{IO}
