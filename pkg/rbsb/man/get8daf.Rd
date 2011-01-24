\name{get8daf}
\alias{get8daf}
\title{
returns the data frame associated to a /daf/
}
\description{
Returns the data frame associated to a /daf/. The row numbers can be
choosen with \code{wh}.
}
\usage{
get8daf(daf, wh=0, strict=FALSE, check=TRUE)
}
\arguments{
\item{daf}{The /daf/ to be read.}
\item{wh}{ numeric giving the numbers of the observations to be read.
Zero means all observations.}
\item{strict}{ When TRUE a fatal error is issued if some asked
observations does not exist. If not the present ones are returned.}
\item{check}{ Must the /daf/ object be checked?}
}
\details{
According to \code{daf@what} a different action is performed to get
the data frame: 

\describe{
\item{f}{ The named \code{daf@valu} function is used without
argument.}
\item{d}{ The named \code{daf@valu} data frame is used.}
\item{t}{ The named \code{daf@valu} file is read with read.table.}
\item{c}{ The named \code{daf@valu} file is read with read.csv.}
\item{c2}{ The named \code{daf@valu} file is read with read.csv2.}}
 For \code{t}, \code{c} or \code{c2} cases, \code{header=TRUE} and
\code{comment.char='#'}.
}
\value{
a data frame
}
\keyword{datasets}
