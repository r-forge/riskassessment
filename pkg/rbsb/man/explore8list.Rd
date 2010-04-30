\name{explore8list}
\alias{explore8list}
\title{
(00) returns the structure of a list in matrix
}
\description{
Explores the branches of a list, returning them into a simple
character matrix: a row for each branch and some columns to describe
some characteristics.\cr The columns are 'number' (the number of this
component from its branch), "numbers" (the succesions of all numbers
leading to this component separated with spaces), "name"
(\eqn{<}NA\eqn{>} if does not exist), "names" (succession of names
separated with spaces), "level" (the branch level), "class" (the
classes of the component separated with spaces).
}
\usage{
explore8list(lili)
}
\examples{
 rs003k("reset");
 uu <- list(A=1:3, 
 B=matrix(letters[1:20], nrow=5, dimnames=list(1:5, c("on", "tw", "th", "fo"))), 
 C=list(a=1:3, b=letters, c=list(final=array(1:8, c(2, 2, 2)))));
 explore8list(uu);
}
\arguments{
\item{lili}{ The list structure to be explored.}
}
\details{
\code{rbsb.sep1} is used to join the names, as a consequence it must
not be present into the names. In case an error is issued.
}
\value{
The resulting character matrix (see the description section).
}
\keyword{IO}
