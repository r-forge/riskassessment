\name{get8daf}
\alias{get8daf}
\title{
(00) returns the data frame associated to a /daf/
}
\description{
The row numbers can be choosen with 'wh'
}
\usage{
get8daf(daf, wh=0, strict=FALSE, check=TRUE)
}
\arguments{
\item{daf}{The /daf/ to be read.}
\item{wh}{(=0) numeric giving the numbers of the observations to be
read. Zero means all observations.}
\item{strict}{(=FALSE) When TRUE a fatal error is issued if some
asked observations does not exist. If not the present ones are
returned.}
\item{check}{(=TRUE) Must the 'daf' object be checked?}
}
\value{
a data frame
}
\keyword{datasets}
