\name{gn2item}
\alias{gn2item}
\title{
returns the item names of a /gn/ object
}
\description{
(see slot @item of /gn/.)
}
\usage{
gn2item(gn, check=FALSE)
}
\examples{
 g4n3k("RESET"); # needed only for R checking, to be forgotten
 gn2item(g4n.gn1);
 gn2item(g4n.gn2);
 gn2item(g4n.gn7);
}
\arguments{
\item{gn}{The gn object.}
\item{check}{Must checking be undertaken (conditionally to
\code{g4n.mck}).}
}
\value{
a character providing the item names.
}
