\name{isalkrepeatable}
\alias{isalkrepeatable}
\title{
indicates if the link is a standard repeatable link
}
\description{
(ba) returns TRUE/FALSE according to fact that this type of link is
repeatable or not.
}
\usage{
isalkrepeatable(ltype)
}
\examples{
 rebastaba3k("RESET"); # needed only for R checking, to be forgotten
 isalkrepeatable("normal");
 isalkrepeatable("program");
}
\arguments{
\item{ltype}{ (character) type of the link.}
}
\value{
TRUE/FALSE
}
