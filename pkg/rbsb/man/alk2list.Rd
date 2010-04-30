\name{alk2list}
\alias{alk2list}
\title{
(ba) transforms a /alk/ object into a list
}
\description{
More or less the inverse operation of \code{list2alk}.
}
\usage{
alk2list(alk)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 alk2list(rbsb.alk0);
 alk2list(rbsb.alk1);
 alk2list(rbsb.alk2);
}
\arguments{
\item{alk}{The alk to be transformed into a list.}
}
\value{
The resulting list.
}
