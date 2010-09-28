\name{char2file}
\alias{char2file}
\title{
writes a character onto a file
}
\description{
The reverse operation of \code{file2char}. This function is not very
tricky, it was written for the completeness.
}
\usage{
char2file(chara, file, path="./", ap=FALSE)
}
\examples{
 rbsb3k("reset");
 char2file(letters, "toto.txt");
 unlink("toto.txt");
}
\arguments{
\item{chara}{ The \code{character} to be written into \code{file}.}
\item{file}{ file to be written. According to \code{ap} when already
exist, it will be destroyed and recreated or completed. }
\item{path}{ Directory containing the file.}
\item{ap}{Must the file be appended if it already exist?}
}
\value{
Nothing but a file is created or modified when everything is right.
}
\keyword{IO}
