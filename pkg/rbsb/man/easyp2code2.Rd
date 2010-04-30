\name{easyp2code2}
\alias{easyp2code2}
\title{
(ba) transforms an easyp expression into an R/bugs block
}
\description{
Numeric values are accepted as easyp expressions. Length(eas) can be
greater than one, in that case a matrix comprising as many columns
will be returned. Rounding and transformation are options.\cr The
code where Bugs code is constructed is not yet available.
}
\usage{
easyp2code2(eas, transfo=TRUE, bugs=FALSE)
}
\examples{
 rsba3k("RESET"); # To comply the R checking
 easyp2code2(1234) # "{cbind(rep(1234, nrow(X)))}" 
 easyp2code2("1234") # "{cbind(rep(1234, nrow(X)))}" 
 easyp2code2("2*pi") # "{cbind(rep(2*pi, nrow(X)))}"
 easyp2code2("1+sqrt({{A}})") # "{1 + sqrt(X[, 'A'])}" 
 easyp2code2("{{*Y*}}+1") # "{Y+1}"
 easyp2code2(11:12) # "{cbind(rep(11, nrow(X)), rep(12, nrow(X)))}"
 easyp2code2("[[2]]") # "{round(Y, 2)}"
 easyp2code2(c("{{A}}", "[[2]]")) # "{cbind(X[, 'A'], round(Y, 2))}"
}
\arguments{
\item{eas}{either a numeric or a rebastaba expression (character).
Its length can be greater than one for repeated standard scalar
distributions or vector parameters of other distributions.}
\item{transfo}{(\code{logical(1)}) Are transformations and Y itself
admitted?}
\item{bugs}{(\code{logical(1)}) Must the code be bugs or R (default).
Not yet available.}
}
\details{
In rebastaba perspective, this function must be called to define the
parameters of the standard nodes like "normal", "Bernoulli",
"multinomial" and so on... But the analysis of the different types of
parents is supposed to be already done, and eas accordingly prepared.
For instance repeated parents are already extended.\cr For each
component it is determined if the expression is vectorial when not a
repetition of \code{nrow(X)} is added as well as a \code{cbind}. When
more than one component, a nesting \code{cbind} is added to produce a
matrix. Vectorial components are identified with the first two rows
of \code{rbsb.cpt} matrix, respectively variable name (with the
special case of \code{rbsb.cni}) and rounding transformation.
}
\value{
An interpretable character string to be included when generating code
(see the following examples).
}
\keyword{utilities}
