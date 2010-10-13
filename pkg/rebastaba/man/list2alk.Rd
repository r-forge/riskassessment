\name{list2alk}
\alias{list2alk}
\title{
transforms a consistent list into a new alk
}
\description{
(ba) Just analyzing the components of the list (consistent names have
to be used) which are supposed to be character and tackle them to
produce consistent slots of an alk object. The produced alk is not
completed since it is defined independently of any bn.
}
\usage{
list2alk(li, name="-?-")
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 list2alk(alk2list(rebastaba.alk0));
 list2alk(alk2list(rebastaba.alk1));
 list2alk(alk2list(rebastaba.alk2));
}
\arguments{
\item{li}{The list to be transformed into an alk object.}
\item{name}{ gives the name of the node when li\$name does not
exist.}
}
\details{
It is worth noticing that there is some shortcuts for defining the
\code{@ldes} slots of the resulting /alk/. (i) The logical way: a
\code{li$ldes} component exists which can be transmitted to the
function \code{list2des}. (ii.a) if not the possible slots (as
\code{@defi}, \code{@orig},...) are available at the first level of
\code{li}. (ii.b) the compulsory slot \code{@name} can be also
present as \code{li$name} or given by the argument \code{name}.\cr
The complete list of standard already programmed distributions is
given with help8ltype function.\cr The main use of this function is
to tackle \code{alk} read from text files with the function
\code{file2list}. It is mainly used in \code{read8bn} function.
}
\value{
The generated 'alk'
}
\keyword{misc}
