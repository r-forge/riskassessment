\name{list2daf}
\alias{list2daf}
\title{
transforms a list into a /daf/
}
\description{
This function transforms a list comprising the necessary components
into a /daf/ object. The components of the list must be either the
direct transcription of the slot hierarchy of /daf/ or a component
named \code{daf} comprising a character(3) with des@name, what and
valu slots.
}
\usage{
list2daf(li)
}
\examples{
 rbsb3k("RESET"); # needed only for R checking, to be forgotten
 lu <- daf2list(new("daf", des=new("des", name="rbsb", 
 orig=paste("Created by rebastaba"), 
 time=date(), 
 defi="prototype", 
 role=character(0), 
 comm=character(0)), 
 what="d", 
 valu="rbsb.dfr0"));
 list2daf(lu);
 list2daf(list(daf=c("simple", "d", "rbsb.dfr0")));
 list2daf(rbsb.lis0);
}
\arguments{
\item{li}{The list to be transformed into a /daf/.}
}
\details{
The list \code{li} can comprise more components which are ignored. If
\code{li$daf} exists, it will have priority over the possible slot
components.
}
\value{
returns the resulting /daf/ object.
}
\keyword{misc}
