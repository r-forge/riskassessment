\name{complete8alk}
\alias{complete8alk}
\title{
completes an alk with respect to a given bn
}
\description{
(bn) This function explores the alk slot to check them and complete
them in such a way that it could be incorporated to the referent bn.
Most often, the /bn/ is under construction and the necessary parent
nodes must have been already integrated.
}
\usage{
complete8alk(bn, alk)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 print(complete8alk(rebastaba.bn2, rebastaba.alk2));
}
\arguments{
\item{bn}{The referent bn object.}
\item{alk}{The alk object to use.}
}
\details{
The principle is not that simple. Some hints are given in the
rebastaba manual. A key point of complet8alk is to finish inheritance
according the properties of parent nodes.
}
\value{
an object of class alk completed (but parentship is provided in
code4bn)
}
