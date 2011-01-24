\name{open8text}
\alias{open8text}
\title{
opens the standard output text for rebastaba
}
\description{
Opens the standard output text for /rbsb/. According to the global
constant \code{rbsb.mfi} the standard output text of rebastaba is
open (in append mode) or not. The name of this file is provided by
the constant \code{rbsb.fou}. Must be called before printing
something ones want to keep on file.
}
\usage{
open8text(append=TRUE)
}
\arguments{
\item{append}{ Must the current file rbsb.fou be continued ?}
}
\details{
Usual derivation is done with \code{sink} until \code{close8text} is
called.
}
\value{
nothing but the indicated actions are performed
}
\keyword{IO}
