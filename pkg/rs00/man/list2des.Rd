\name{list2des}
\alias{list2des}
\title{
(f2) transforms a consistent list into a new des object
}
\description{
Just analyzing the components of the list (consistent names have to
be used) which are supposed to be character and tackle them to
produce consistent slots of a /des/
}
\usage{
list2des(li, name=rbsb3k("nch"))
}
\examples{
 print(list2des(list(name="A", role="Just to see", comm="Not very interesting")));
}
\arguments{
\item{li}{The list to be transformed into a des object.}
\item{name}{(=rbsb3k("nch")) gives the name of the node when li\$name
does not exist. If both are absent then an error is issued.}
}
\details{
The main use of this function is to tackle alk read from text files
with the function read8list avoiding the NULL values non acceptable
for slots
}
\value{
The generated 'des' object
}
\keyword{classes}
