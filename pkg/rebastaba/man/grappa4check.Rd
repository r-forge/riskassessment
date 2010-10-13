\name{grappa4check}
\alias{grappa4check}
\title{
checks if the provided bn is Grappa-compatible
}
\description{
(bn) More precisely, all nodes must be of numcat type.
}
\usage{
grappa4check(bn)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 grappa4check(rebastaba.bn2);
 uu <- and4bn(zero2bn(char2des("oui")), new8alk(char2des("NC1"), ltype="numcat", 
 lpara=list(p=1:4), lpod=list(LETTERS[1:4])));
 grappa4check(uu);
}
\arguments{
\item{bn}{The bn object.}
}
\value{
TRUE or FALSE according to
}
