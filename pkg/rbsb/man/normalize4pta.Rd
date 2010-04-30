\name{normalize4pta}
\alias{normalize4pta}
\title{
(ba) normalize pt
}
\description{
Whatever are the values given in the @pro they are normalized and
rounded to be printed according to its slot @kkk as explained in the
details of print8pta
}
\usage{
normalize4pta(pt)
}
\examples{
 rsba3k("RESET"); # needed only for R checking, to be forgotten
 print(normalize4pta(rbsb.pta0));
}
\arguments{
\item{pt}{The pt to be normalized.}
}
\value{
the pt after normalization
}
\keyword{utilities}
