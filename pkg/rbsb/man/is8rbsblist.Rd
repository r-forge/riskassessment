\name{is8rbsblist}
\alias{is8rbsblist}
\title{
checks whether a list is rbsb-compatible list or not
}
\description{
A rbsb-list must satisfy the following two properties: 

\describe{
\item{i}{All components and sub-components being named.}
\item{ii}{All components and sub-components being either a list or a
character vector/matrix/array (vma components are called leafs of the
rbsb-list).}}
}
\usage{
is8rbsblist(lili)
}
\examples{
 rbsb3k("reset");
 is8rbsblist(rbsb.lis1);
 is8rbsblist(list(rbsb.lis1));
 print(rbsb.is8rbsblist, what="c");
 is8rbsblist(list(rbsb.lis1, nu=NULL));
 print(rbsb.is8rbsblist, what="c");
}
\arguments{
\item{lili}{ The list structure to be checked.}
}
\value{
TRUE/FALSE according to the case. Also a global variable
\code{rbsb.is8rbsblist} is created to give the result through a
\code{des} object.
}
\keyword{IO}
