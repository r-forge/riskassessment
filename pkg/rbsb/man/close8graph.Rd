\name{close8graph}
\alias{close8graph}
\title{
closes the file open by open8graph
}
\description{
According to the global constant \code{rbsb.mfi} closes the file open
by \code{open8graph}. Also if \code{!rbsb.mba} a pause is issued.
}
\usage{
close8graph(message=rbsb.cha0)
}
\arguments{
\item{message}{ Message to display on the terminal. When empty, no
message will be displayed.}
}
\value{
nothing but the actions indicated in the description field are
performed
}
\keyword{IO}
