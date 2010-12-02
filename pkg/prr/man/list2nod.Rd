\name{list2nod}
\alias{list2nod}
\title{
transforms a consistent list into a new nod object
}
\description{
Just analyzing the components of the list (consistent names have to
be used) which are supposed to be character and tackle them to
produce consistent slots of a /nod/ object. The main use of this
function is to generate chains read from text files with the function
\code{read8chain}.
}
\usage{
list2nod(li)
}
\examples{
 prr3k("RESET"); # (only for R checking)
 print(list2nod(nod2list(prr.nod1)));
}
\arguments{
\item{li}{The list to be transformed into a /nod/ object.}
}
\details{
Non used components of the list are not considered, this is the case
for a possible \code{$name}.
}
\value{
The generated 'nod' object
}
