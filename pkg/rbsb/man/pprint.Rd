\name{pprint}
\alias{pprint}
\title{
double print at the screen and into a text file
}
\description{
Just doing a double printing (i) on the screen and (ii) appending to
the file fifi
}
\usage{
pprint(x, fifi="", ...)
}
\examples{
 pprint("to see what happen!", "toto.txt");
 read.table("toto.txt");
 unlink("toto.txt");
}
\arguments{
\item{x}{ The object to print.}
\item{fifi}{ Name of the file, if '' the print is only done onto the
screen.}
\item{\dots}{additional arguments for the print call.}
}
\details{
The file must reopen to empty it if necessary
}
\value{
nothing is returned
}
\keyword{IO}
