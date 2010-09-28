\name{form3norma}
\alias{form3norma}
\title{
removes redundant characters into a character string
}
\description{
The most immediate use (default) is to transform sequences of \code{"
"} into a simple \code{" "}. More generally, removes all sequences of
\code{redu} before \code{bef} or after \code{aft}.
}
\usage{
form3norma(cha, redu="", bef="", aft="")
}
\examples{
 rbsb3k("reset"); # only necessary for R checking
 form3norma(" pour voir ");
 form3norma(c(" A > B ", "B > C", "D"), bef=">");
 form3norma(c(" A > B ", "B > C", "D"), bef=">", aft=">");
}
\arguments{
\item{cha}{ The character to normalize, can be of length greater than
one.}
\item{redu}{ The character to remove, single or not.}
\item{bef}{ The ante-tag, single or not.}
\item{aft}{ The post-tag, single or not.}
}
\value{
The transformed character (with the same length than \code{cha}.
}
\keyword{IO}
