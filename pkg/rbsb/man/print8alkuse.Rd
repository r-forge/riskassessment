\name{print8alkuse}
\alias{print8alkuse}
\title{
(ba) prints an alk object not supposed to be completed for a bn
}
\description{
This function prints in a interpreted way an alk object supposed not
to be completed. Must not be called by the user who might use
directly the \code{print} associated to \code{/alk/}.
}
\usage{
print8alkuse(alk, des=0, retrait=4)
}
\arguments{
\item{alk}{The alk object.}
\item{des}{(=0) indicates if the description slot must be printed. O:
no; 1: in a short way; 2: completely. Other values means nothing is
printed.}
\item{retrait}{(=4) number of spaces for indentation.}
}
\details{
The interpretation is hand made for each type of links.
}
\value{
nothing but a print is performed
}
\keyword{print}
