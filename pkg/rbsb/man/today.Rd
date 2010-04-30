\name{today}
\alias{today}
\title{
(00) returns a character giving the present day
}
\description{
returns a character giving the present day The day is given as
aa\_mm\_dd numbers
}
\usage{
today(format="red")
}
\examples{
 cat("Today is", today(), "\n");
}
\arguments{
\item{format}{"red" for reduced.}
}
\details{
based on Sys.Date R function
}
\value{
a character
}
\keyword{utilities}
