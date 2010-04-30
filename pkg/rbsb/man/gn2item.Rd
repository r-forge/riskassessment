\name{gn2item}
\alias{gn2item}
\title{
(gn) returns the item names of a /gn/ object
}
\description{
(see slot @item of /gn/.)
}
\usage{
gn2item(gn, check=FALSE)
}
\examples{
 rsgn3k("RESET"); # needed only for R checking, to be forgotten
 gn2item(rbsb.gn1);
 gn2item(rbsb.gn2);
 gn2item(rbsb.gg1);
}
\arguments{
\item{gn}{The gn object.}
\item{check}{Must checking be undertaken (conditionally to
\code{rbsb.mck}).}
}
\value{
a character providing the item names.
}
