\name{name8list}
\alias{name8list}
\title{
(00) returns the same list but with names
}
\description{
Explores the branches of a list, returning the same valued list but
adding names to the components if not existing. This is a way to
comply one of the two requirements to be a rbsb-list (see
\code{is8rbsblist}.
}
\usage{
name8list(lili)
}
\examples{
 rs003k("reset");
 uu <- list(list(1:3, 4:6), 
 B=matrix(letters[1:20], nrow=5, dimnames=list(1:5, c("on", "tw", "th", "fo"))), 
 list(a=1:3, b=letters, list(array(1:8, c(2, 2, 2)))));
 name8list(uu);
}
\arguments{
\item{lili}{ The list structure to be possibly named.}
}
\value{
The resulting named list.
}
\keyword{IO}
