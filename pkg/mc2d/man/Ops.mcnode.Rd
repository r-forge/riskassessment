\name{Ops.mcnode}
\alias{Ops.mcnode}
\title{
Operations on mcnode Objects
}
\description{
This function alters the way operations are performed on
\code{mcnode} objects for a better consistancy of the theory.
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
\item{e1}{An \code{mcnode} object, a vector or an array.}
\item{e2}{An optionnal \code{mcnode} object, a vector or a matrix
with at least one of both objects as an \code{mcnode}.}
}
\details{
This method will be used for any of the Group \code{\link{Ops}}
functions, i.e.: 

\itemize{
\item \code{"+"}, \code{"-"}, \code{"*"}, \code{"/"}, \code{"^"},
\code{"\%
\%
"}, \code{"\%
/\%
"}
\item \code{"&", "|", "!"}
\item \code{"==", "!=", "<", "<=", ">=", ">"}}
The rules are as following (illustrated with a \code{"+"} function
and ignoring the \code{nvariates} dimension): 

\itemize{
\item \code{"0" + "0" = "0"};
\item \code{"0" + "V" = "V"}: classical recycling of the scalar;
\item \code{"0" + "U" = "U"}: classical recycling of the scalar;
\item \code{"0" + "VU" = "VU"}: classical recycling of the scalar;
\item \code{"V" + "V" = "V"}: if both of the same \code{(nsv)}
dimension;
\item \code{"V" + "U" = "VU"}: the \code{"U"} object will be recycled
"by row". The \code{"V"} object will be recycled classically "by
column";
\item \code{"V" + "VU" = "VU"}: if the dimension of the \code{"V"} is
\code{(nsv)} and the dimension of the \code{"VU"} is \code{(nsv x
nsu)}. The \code{"V"} object will be recycled classically "by
column";
\item \code{"U" + "U" = "U"}: if both of the same \code{(nsu)}
dimension;
\item \code{"U" + "VU" = "VU"}: if the dimension of the \code{"U"} is
\code{(nsu)} and the dimension of the \code{"VU"} is \code{(nsv x
nsu)}. The \code{"U"} object will be recycled "by row";
\item \code{"VU" + "VU" = "VU"}: if the dimension of the \code{"VU"}
nodes is \code{(nsu x nsv)};}
A vector or an array may be combined with an \code{mcnode} of size
\code{(nsv x nsu)} if an \code{mcnode} of this dimension may be built
from this vector/array using the \code{mcdata} function. See
\code{\link{mcdata}} for the rules.

The \code{outm} attribute is transferred as following: \code{"each" +
"each" = "each"}; \code{"none" + other = other}; \code{other1 +
other2 = other1}. The \code{outm} attribute of the resulting node may
be changed using the \code{\link{outm}} function.

For multivariate nodes, a recycling on the \code{nvariates} dimension
is done if a \code{(nsu x nsv x nvariates)} node is combined with a
\code{(nsu x nsv x 1)} node.
}
\value{
The results as a \code{mcnode} object.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{mcdata}}, \code{\link{mcstoc}}
}
\keyword{utilities}
