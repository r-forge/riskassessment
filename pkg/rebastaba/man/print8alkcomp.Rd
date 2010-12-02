\name{print8alkcomp}
\alias{print8alkcomp}
\title{
prints an alk object supposed to be completed for a bn
}
\description{
(ba) This function prints in a interpreted way an alk object supposed
to be completed.. Must not be called by the user who might use
directly the \code{print} associated to \code{/alk/}.
}
\usage{
print8alkcomp(alk, des=0, retrait=4)
}
\arguments{
\item{alk}{The alk object.}
\item{des}{ indicates if the description slot must be printed. O: no;
1: in a short way; 2: completely. Other values means that nothing is
printed.}
\item{retrait}{ number of spaces for indentation.}
}
\details{
The interpretation is hand made for each type of links. The
interpretation is hand made for each type of links. The node name is
not given because it could be more convenient to print it outside
this function with some adequate format.
}
\value{
nothing but a print is performed
}
\keyword{print}
