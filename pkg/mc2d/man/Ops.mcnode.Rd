\name{Ops.mcnode}
\alias{Ops.mcnode}
\title{
Operations on mcnode Objects
}
\description{
This function alters the way operations are performed on mcnode
objects for a better consistancy of the theory.
}
\usage{
\method{Ops}{mcnode}(e1, e2)
}
\examples{
oldvar <- ndvar()
oldunc <- ndunc()
ndvar(30)
ndunc(20)

## Given
x0 <- mcdata(3, type="0")
xV <- mcdata(1:ndvar(), type="V")
xU <- mcdata(1:ndunc(), type="U")
xVU <- mcdata(1:(ndunc()*ndvar()), type="VU")
x0M <- mcdata(c(5, 10), type="0", nvariates=2)
xVM <- mcdata(1:(2*ndvar()), type="V", nvariates=2)
xUM <- mcdata(1:(2*ndunc()), type="U", nvariates=2)
xVUM <- mcdata(1:(2*(ndunc()*ndvar())), type="VU", nvariates=2)

## All possible combinations
## "0"
-x0
x0 + 3

## "V"
-xV
3 + xV
xV * (1:ndvar())
xV * x0
xV - xV

## "U"
-xU
xU + 3
(1:ndunc()) * xU
xU * x0
xU - xU

## Watch out the resulting type
xV + xU
xU + xV

## "VU"
-xVU
3 + xVU
(1:(ndunc()*ndvar())) * xVU
xVU + xV
x0 + xVU
xU + xVU
xVU - xVU

## Some Multivariates
x0M+3
xVM * (1:ndvar())
xVM - xV
xUM - xU
xVUM - xU
}
\arguments{
\item{e1}{An mcnode object, a vector or an array.}
\item{e2}{An optionnal mcnode object, a vector or a matrix with at
least one of both objects as an mcnode.}
}
\details{
This method will be used for any of the Group \code{\link{Ops}}
functions, i.e.: 

\itemize{
\item "+", "-", "*", "/", "\eqn{\mbox{\textasciicircum}}{^}", "\%
\%
","\%
/\%
"
\item "\&", "\eqn{|}", "!"
\item "==", "!=", "\eqn{<}", "\eqn{<}=", "\eqn{>}=", "\eqn{>}"}
The rules are as following (illustrated with a "+" function and
ignoring the nvariates dimension): 

\itemize{
\item "0" + "0" = "0";
\item "0" + "V" = "V": classical recycling of the scalar;
\item "0" + "U" = "U": classical recycling of the scalar;
\item "0" + "VU" = "VU": classical recycling of the scalar;
\item "V" + "V" = "V": if both of the same (nsv) dimension;
\item "V" + "U" = "VU": the "U" object will be recycled "by row". The
"V" object will be recycled classically "by column";
\item "V" + "VU" = "VU": if the dimension of the "V" is (nsv) and the
dimension of the "VU" is (nsv x nsu). The "V" object will be recycled
classically "by column";
\item "U" + "U" = "U": if both of the same (nsu) dimension;
\item "U" + "VU" = "VU": if the dimension of the "U" is (nsu) and the
dimension of the "VU" is (nsv x nsu). The "U" object will be recycled
"by row";
\item "VU" + "VU" = "VU": if the dimension of the "VU" nodes is (nsu
x nsv);}
A vector or an array may be combined with an mcnode of size (nsv x
nsu) if an mcnode of this dimension may be built from this
vector/array using the mcdata function. See \code{\link{mcdata}} for
the rules.

The outm attribute is transferred as following: "each" + "each" =
"each"; "none" + other = other; other1 + other2 = other1. The outm
attribute of the resulting node may be changed using the
\code{\link{outm}} function.

For multivariate nodes, a recycling on the nvariates dimension is
done if a (nsu x nsv x nvariates) node is combined with a (nsu x nsv
x 1) node.
}
\value{
The results as a mcnode object.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{mcdata}}, \code{\link{mcstoc}}
}
\keyword{utilities}
