\name{today}
\alias{today}
\title{
(f0) returns a character giving the present day
}
\description{
returns a character giving the present day The day is given as
aa\_mm\_dd numbers
}
\usage{
today(format=c("red", "nor"))
}
\examples{
 cat("Today is", today(), "\n");
}
\arguments{
\item{format}{"red" for reduced and "nor" for normal.}
}
\details{
based on Sys.Date R function
}
\value{
a character
}
\keyword{utilities}
