\name{rebastaba3k}
\alias{rebastaba3k}
\title{
assigns the rsba constants
}
\description{
(ba) defines or returns the constants used within /rbsbba/. The
performed action depends on the argument.
}
\usage{
rebastaba3k(whi)
}
\examples{
 rebastaba3k("RESET");
 print(g4n.gn1);
 rebastaba3k("names");
 rebastaba3k("values");
}
\arguments{
\item{whi}{a character(1) indicating either to reset or to return the
names or the current values. The three possible values are
\code{RESET}, \code{reset}, \code{names}, \code{definitions} or
\code{values}.}
}
\details{
see the details in \code{rbsb3k} NEVERTHELESS be aware that changing
some constants like \code{rbsb.nom?} will possibly imply
inconsistency with the constants of other layers, like
\code{g4n.gn?}, \code{rebastaba.bn?}, \code{rebastaba.dn?}...
}
\value{
When \code{whi=="RESET"} nothing (but the assignments are performed
for all layers \code{rbsb} and \code{rsba}). When \code{whi=="reset"}
nothing (but the assignments of the layer \code{rsba} are performed).
When \code{whi=="names"} the names as a character vector. When
\code{whi=="definitions"} the definitions as a named character
vector. When \code{whi=="values"} the values through a named list.
}
\keyword{misc}
