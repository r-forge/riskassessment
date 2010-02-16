\name{comp3dtriang}
\alias{comp3dtriang}
\title{
(00) develops a delay
}
\description{
returns a list with the days and proportions to apply to each of
them. When \code{ddel} and \code{ste} are interger, the \code{$xval}
are also integer.
}
\usage{
comp3dtriang(ddel, ste=1, pos=TRUE)
}
\arguments{
\item{ddel}{(\code{numeric(3)}) Definition of the delay (first day,
maximum day, last day).}
\item{ste}{(\code{numeric(1)}) Definition of the progression.}
\item{pos}{(\code{logical(1)}) When TRUE the window is first expanded
of \code{ste} each side to give non null \code{$xval}.}
}
\details{
The function \code{seq} is used to compute \code{$xval}.
}
\value{
A list with two components. \code{$xval} gives the values;
\code{prop} gives the proportion to apply to each of them
(\code{sum($prop)=1}.
}
\keyword{misc}
