\name{form3file}
\alias{form3file}
\title{
writes a character vector to a file
}
\description{
The aim of this function is generate files from successive character
vectors. According to \code{append} the possibly existing file is
appended or newly created. As many lines as components to
\code{chain} are introduced to the file.
}
\usage{
form3file(chaine, file="", append=TRUE)
}
\examples{
 form3file(letters, "useless.txt")
}
\arguments{
\item{chaine}{the character string to be output}
\item{file}{ file where to write it down. When \code{""}, the result
is displayed on the screen.}
\item{append}{ Indicates if appending or not to \code{file}.}
}
\details{
\\n are added to create the new lines (even when file is \code{""}).
}
\value{
nothing but the file is written
}
\keyword{IO}
