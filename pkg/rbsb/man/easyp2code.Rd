\name{easyp2code}
\alias{easyp2code}
\title{
(bn) transforms an easyp expression into an R/bugs block
}
\description{
Numeric values are accepted as easyp expressions. eas can be a vector
to encompass repeated variables. Access to the bn component allow to
know if parent nodes are repeated or not, but the number of
repetitions of the dealt node is supposed already established.
Rounding and transformation are options.
}
\usage{
easyp2code(eas, bn, iin, transfo=FALSE, bugs=FALSE)
}
\examples{
 rsbn3k("RESET"); # (only for R checking)
 cat(easyp2code("{{A}}", rbsb.bn2, 2), "\n");
 cat(easyp2code(1234, rbsb.bn2, 2), "\n");
 cat(easyp2code("2*pi", rbsb.bn2, 2), "\n");
 cat(easyp2code("1+sqrt({{A}})", rbsb.bn2, 2), "\n");
 cat(easyp2code("1+sqrt({{A}}*{{B}})", rbsb.bn2, 2), "\n");

}
\arguments{
\item{eas}{either a numeric or a rebastaba expression (character).
Its length can be greater than one for repeated standard scalar
distributions or vector parameters of other distributions.}
\item{bn}{The bn giving the context of the translation.}
\item{iin}{internal numbering of the bn node concerned with the easyp
object.}
\item{transfo}{(=FALSE) Are rounding and transformation accepted?}
\item{bugs}{(=FALSE) Must the code be bugs or R (default).}
}
\details{
This function must be called to define the parameters of the standard
links like "normal", "Bernoulli", "multinomial" and so on... The bn
is not finished, so it is not checked but the dertermination of
repetition is supposed to be done [repetition \eqn{<}=\eqn{>}
(convenient distribution \& (length(lvar)\eqn{>}1))]. Notice that the
slots @par \& @lvar of the iin-th node are supposed already
completed.
}
\value{
An interpretable character string to be included when generating
code.
}
\keyword{utilities}
