\name{now}
\alias{now}
\title{
returns a character giving the present moment
}
\description{
returns a character giving the present moment including the day given
as aa\_mm\_dd numbers
}
\usage{
now(format="red")
}
\examples{
 cat("Now is", now(), "\n");
}
\arguments{
\item{format}{"red" for reduced.}
}
\details{
based on Sys.time R function
}
\value{
a character
}
\keyword{utilities}
