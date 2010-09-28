\name{form3file}
\alias{form3file}
\title{
adds a character vector into a file
}
\description{
The aim of this function is generate files from a character vector.
In the framework of rebastaba, the production of files to be dealt by
external programs.
}
\usage{
form3file(chaine, file="", append=TRUE)
}
\examples{
 form3file(letters, "useless.txt")
}
\arguments{
\item{chaine}{the character string to be output}
\item{file}{ file where to write it down.}
\item{append}{ Indicates if appending or not.}
}
\value{
nothing but the file is written
}
\keyword{IO}
