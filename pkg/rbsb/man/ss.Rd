\name{ss}
\alias{ss}
\title{
sources an R file
}
\description{
Just sourcing \code{file} after adding a \code{directory} path and
emmetting a \code{message}.
}
\usage{
ss(file, message=rbsb.cha0, directory=rbsb.cha0, ...)
}
\arguments{
\item{file}{ The R file to be sourced.}
\item{message}{An indicating message to display, possibly with a
pause according to \code{rbsb.mba}.}
\item{directory}{ The directory path to add before the file.}
\item{\dots}{Further arguments to be passed to the source function.}
}
\value{
nothing but the actions indicated in the description field are
performed
}
