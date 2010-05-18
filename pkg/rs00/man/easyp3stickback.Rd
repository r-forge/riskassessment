\name{easyp3stickback}
\alias{easyp3stickback}
\title{
(00) converse operation of easyp3cut
}
\description{
d must be a list with \$typ and \$blo components (see easyp3cut for
the details)
}
\usage{
easyp3stickback(d, pth=matrix(c("(", "{", "[", ")", "}", "]"), 3))
}
\examples{
 rs003k("RESET"); # for R checking compliance (useless)
 uu <- easyp3cut("abs({Y})*[A]^(B)");
 easyp3stickback(uu);
}
\arguments{
\item{d}{d\$typ for the type of components, d\$blo for the content of
each component.}
\item{pth}{character matrix with two columns defining the opening
parentheses in the first column and the closing parentheses in the
second column.}
}
\value{
A character(1) of the reconstituted easyprograming
}
\keyword{utilities}
