\name{list2alk}
\alias{list2alk}
\title{
(ba) transforms a consistent list into a new alk
}
\description{
Just analyzing the components of the list (consistent names have to
be used) which are supposed to be character and tackle them to
produce consistent slots of an alk object. The produced alk is not
completed since it is defined independently of any bn.\cr It is worth
noticing that the slots for the description are at the same level.
}
\usage{
list2alk(li, name=rbsb.cha0)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 #list2alk(alk2list(rbsb.alk0));
 #list2alk(alk2list(rbsb.alk1));
 #list2alk(alk2list(rbsb.alk2));
}
\arguments{
\item{li}{The list to be transformed into an alk object.}
\item{name}{(=rbsb.cha0) gives the name of the node when li\$name
does not exist. If both are absent then an error is issued.}
}
\details{
The complete list of standard already programmed distributions is
given with help8ltype function.\cr The main use of this function is
to tackle alk read from text files with the function
\code{file2list}.
}
\value{
The generated 'alk'
}
