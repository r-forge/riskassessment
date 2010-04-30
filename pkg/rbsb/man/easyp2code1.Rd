\name{easyp2code1}
\alias{easyp2code1}
\title{
(bn) expands an easyp expression
}
\description{
taking care of the parentship of the node iin of /bn/ bn, and of the
dimensions of the involved node, expands the easyp expression in
order that the function easyp2code2 be able to translate it without
need of the bn.
}
\usage{
easyp2code1(eas, bn, iin, transfo=FALSE, bugs=FALSE)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 easyp2code1("{{A}}", rbsb.bn2, 2);
 easyp2code1(1234, rbsb.bn2, 2);
 easyp2code1("1234", rbsb.bn2, 2);
 easyp2code1("2*pi", rbsb.bn2, 2);
 easyp2code1("1+sqrt({{A}})", rbsb.bn2, 2);
 easyp2code1("1+sqrt({{A}}*{{B}})", rbsb.bn2, 2);
 easyp2code1(c(1234, 5678), rbsb.bn2, 2);
 \dontrun{easyp2code1(c("{{A}}", "{{B}}"), rbsb.bn2, 2);}
}
\arguments{
\item{eas}{The easyp expression to deal with.}
\item{bn}{The bn giving the context of the expansion.}
\item{iin}{internal numbering of the bn node concerned with the easyp
object.}
\item{transfo}{(=FALSE) Are rounding and transformation accepted?}
\item{bugs}{(=FALSE) Must the code be bugs or R (default).}
}
\details{
The type of the node is of importance for the expansion, then when a
new node type is added, this function must be completed accordingly.
See also the comments of the easyp2code function.\cr Notice that
rbsb.cni node is escaped.
}
\value{
An interpretable character string to be proposed to easyp2code2
}
\keyword{utilities}
