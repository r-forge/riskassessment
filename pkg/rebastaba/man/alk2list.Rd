\name{alk2list}
\alias{alk2list}
\title{
transforms a /alk/ object into a list
}
\description{
(ba) More or less the inverse operation of \code{list2alk}.
}
\usage{
alk2list(alk)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 alk2list(rebastaba.alk0);
 alk2list(rebastaba.alk1);
 alk2list(rebastaba.alk2);
}
\arguments{
\item{alk}{The alk to be transformed into a list.}
}
\value{
The resulting list.
}
