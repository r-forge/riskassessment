\name{alkvar}
\alias{alkvar}
\title{
returns the variable names of an /alk/
}
\description{
(ba) Whatever is the state (completed or not) of an /alk/ returns the
names of its variable names.
}
\usage{
alkvar(alk)
}
\examples{
 rebastaba3k("RESET"); # for R checking
 alkvar(rebastaba.alk2)
}
\arguments{
\item{alk}{The alk object}
}
\details{
No check, this function is to be used for checking functions
}
\value{
A \code{character} containing the natures of the variables
}
