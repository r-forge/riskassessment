\name{list2gn}
\alias{list2gn}
\title{
transforms a consistent list into a new /gn/ object
}
\description{
Just analyzing the components of the list (consistent names have to
be used) which are supposed to be character and tackle them to
produce consistent slots of a /gn/ object... The main use of this
function is to generate /gn/ read from text files with the function
\code{read8gn}.
}
\usage{
list2gn(li)
}
\examples{
 g4n3k("RESET"); # for R checking need
 print(list2gn(list(name="a", item=c("A", "B"))));
}
\arguments{
\item{li}{The list to be transformed into a /gn/ object. Compulosory
component is \code{$name}. Usual component is \code{$arc}. Possible
components are \code{$posi} for the positions, \code{$item} for the
node/variabl}
}
\details{
When \code{$item} does not exist, \code{@item} is computed from
\code{$posi} if it exists if not from \code{$arc}.
}
\value{
The generated /gn/ object
}
