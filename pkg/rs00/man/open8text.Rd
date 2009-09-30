\name{open8text}
\alias{open8text}
\title{
(f0) opens the standard output text for rebastaba
}
\description{
According to the global constant rbsb3k("mfi") the standard output
text of rebastaba is open (in append mode) or not. The name of this
file is provided by the constant rbsb3k("fou"). Must be called before
printing something ones want to keep on file.
}
\usage{
open8text(append=TRUE)
}
\arguments{
\item{append}{ Must the current file rbsb3k("fou") be continued ?}
}
\value{
nothing but the actions indicated in the description field are
performed
}
\keyword{IO}
