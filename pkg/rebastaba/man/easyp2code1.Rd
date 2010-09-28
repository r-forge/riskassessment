\name{easyp2code1}
\alias{easyp2code1}
\title{
expands an easyp expression
}
\description{
(bn) Expands an easyp expression in the context precised by
\code{nom} for the dimensions. The aim is to give the function
easyp2code2 an easier input. Mainly checks are performed about the
consistency between the proposed \code{eas} and the involved parents.
Also \code{eas} is vectorized when its parentship asks for it.
}
\usage{
easyp2code1(eas, nom, iin, transfo=FALSE, bugs=FALSE)
}
\examples{
 rebastaba3k("RESET"); # (only for R checking)
 easyp2code1("{{A}}", rbsb.nom2, 2);
 easyp2code1(1234, rbsb.nom2, 2);
 easyp2code1("1234", rbsb.nom2, 2);
 easyp2code1("2*pi", rbsb.nom2, 2);
 easyp2code1("1+sqrt({{A}})", rbsb.nom2, 2);
 easyp2code1("1+sqrt({{A}}*{{B}})", rbsb.nom2, 2);
 easyp2code1(c(1234, 5678), rbsb.nom2, 2);
 \dontrun{easyp2code1(c("{{A}}", "{{B}}"), rbsb.nom2, 2);}
}
\arguments{
\item{eas}{The easyp expression to deal with.}
\item{nom}{The /nom/ giving the context of the expansion.}
\item{iin}{node number concerned with \code{eas}.}
\item{transfo}{ Are rounding and transformation accepted?}
\item{bugs}{ Must the code be bugs or R (not yet implemented).}
}
\details{
Notice that rbsb.cni node is escaped from the variable/node names.
Also that the node dimension and possible variable was already
determined before calling the function.
}
\value{
An interpretable character string to be proposed to easyp2code2
}
\keyword{utilities}
