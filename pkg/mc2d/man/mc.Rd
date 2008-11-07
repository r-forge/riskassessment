\name{mc}
\alias{mc}
\title{
Monte Carlo Object
}
\description{
Creates \code{mc} objects from \code{\link{mcnode}} or \code{mc}
objects.
}
\usage{
mc(..., name=NULL, devname=FALSE)
}
\examples{
 
x <- mcstoc(runif)
y <- mcdata(3, type="0")
z <- x * y
(m <- mc(x, y, z, name=c('n1', 'n2', 'n3')))
mc(m, x, devname=TRUE)
}
\arguments{
\item{...}{\code{mcnode} and/or \code{mc} object(s) to be gathered in
a \code{mc} object separated by a coma.}
\item{name}{Vector of character of the same length of the final
\code{mc} object. If NULL, the name will be given from the name of
the elements. }
\item{devname}{Develop the name from the name of the \code{mc}
objects, if any.}
}
\details{
A \code{mc} object is a list of \code{\link{mcnode}} objects.
\code{mcnode} objects must be of coherent dimensions.

If one of the arguments is a \code{mc} object, the name of the
elements of this \code{mc} object are used. \code{devname = TRUE}
will develop the name, using as a prefix the name of the \code{mc}
object.

 Finally, names are transformed to be unique.
}
\value{
An object of class \code{mc}.
}
\author{
Regis Pouillot
}
\seealso{
\code{\link{mcnode}}, the basic element of a \code{mc} object.

 To evaluate \code{mc} objects: \code{\link{mcmodel}},
\code{\link{evalmcmod}}, \code{\link{evalmccut}}

 Informations about an \code{mc} object: \code{\link{is.mc}},
\code{\link{dimmc}}

 To study \code{mc} objects: \code{\link{print.mc}},
\code{\link{summary.mc}}, \code{\link{plot.mc}},
\code{\link{converg}}, \code{\link{hist.mc}}, \code{\link{tornado}},
\code{\link{tornadounc.mc}}
}
\keyword{methods}
