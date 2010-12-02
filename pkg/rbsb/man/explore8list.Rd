\name{explore8list}
\alias{explore8list}
\title{
returns the structure of a list in matrix
}
\description{
Recursively explores the branches of a list, returning them into a
character matrix: a row for each branch, the columns describing some
characteristics.\cr The columns are 'number' (the number of this
component from its branch), 'numbers' (the succesions of all numbers
leading to this component separated with spaces), 'name'
(\eqn{<}NA\eqn{>} if does not exist), 'names' (succession of names
separated with spaces), 'level' (the branch level, i.e. a numbering
of the branches), 'depth' (the branch depth from the root,
implicitely 0 for the root), 'class' (the classes of the component
separated with spaces).
}
\usage{
explore8list(lili)
}
\examples{
 rbsb3k("reset");
 explore8list(rbsb.lis1);
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
