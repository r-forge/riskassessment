\name{easyp2code2}
\alias{easyp2code2}
\title{
(f0) transforms an easyp expression into an R/bugs block
}
\description{
Numeric values are accepted as easyp expressions. Length(eas) can be
greater than one, in that case a matrix comprising as many columns
will be returned. Repetition cases is supposed to be already
introduced with more than one component. Rounding and transformation
are options.\cr The code where Bugs code is constructed must be
updated
}
\usage{
easyp2code2(eas, transfo=FALSE, bugs=FALSE)
}
\arguments{
\item{eas}{either a numeric or a rebastaba expression (character).
Its length can be greater than one for repeated standard scalar
distributions or vector parameters of other distributions.}
\item{transfo}{ Are rounding and transformation accepted? In that
case, correspond elements are replaced with Y or Y[,i] according to
the length of eas.}
\item{bugs}{ Must the code be bugs or R (default). To be done.}
}
\details{
In rebastaba perspective, this function must be called to define the
parameters of the standard nodes like "normal", "Bernoulli",
"multinomial" and so on... But the analysis of the different types of
parents is supposed to be already done, and eas accordingly prepared.
For instance repeated parents are already extended. This gives the
opportunity for easyp2code2 to belong to the zero (independent from
/rbsb/ objects) level.\cr Checking of parentship and so on are
supposed to be already done before the calling to easyp2code2. Also
is supposed already detected and expanded the necessary
repetitions.\cr Notice that the code includes some constant for other
uses.
}
\value{
An interpretable character string to be included when generating code
(see the following examples).
}
\keyword{utilities}
