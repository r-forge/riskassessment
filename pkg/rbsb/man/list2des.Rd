\name{list2des}
\alias{list2des}
\title{
transforms a consistent list into a /des/ object
}
\description{
Just analyzing the components of the list (consistent names have to
be used) which are supposed to be character and tackle them to
produce consistent slots of a /des/ object. The components of the
list must be either the direct transcription of the slots of /des/ or
a component named \code{des} comprising a \code{character(1:Inf)}
with des@name, des@orig, des@time, des@defi,des@role,des@comm in this
order.
}
\usage{
list2des(li)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 print(list2des(list(name="A", role="Just to see", comm="Not very interesting")));
 print(list2des(list(des=c("A", "", "", "", "Just to see", "Not very interesting"))));
}
\arguments{
\item{li}{The list to be transformed into a des object.}
}
\value{
The generated 'des' object
}
\keyword{classes}
